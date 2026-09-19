// Code-behind for Tournaments.razor.
//
// The markup and the logic were one 4000-line file, of which 3700 lines were a single @code
// block. Splitting them changes nothing at runtime - Blazor compiles a component's @code into
// exactly this partial class - but it gives the C# a file an editor can navigate, and it lets
// the two halves be reviewed apart. The .razor file keeps the markup and the directives, so
// @inject stays where it is and the injected members are visible here as usual.
//
// Further partials live beside this file, split by what they are responsible for.

using System.Diagnostics;
using System.Text.Json;
using ChessLibrary;
using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.Web;
using Microsoft.JSInterop;
using MudBlazor;
using Toolbelt.Blazor.HotKeys2;
using WebGUI.Components.Layout;
using WebGUI.Components.Layout.ChessboardLayout;
using WebGUI.Components.Layout.CrosstableLayout;
using WebGUI.Components.Layout.TournamentLayout;
using WebGUI.Components.Pages.Experimental;
using WebGUI.Plotting;
using WebGUI.Services;
using static ChessLibrary.Configuration;
using static ChessLibrary.EngineProtocol;
using static ChessLibrary.EngineTypes;
using static ChessLibrary.GameAnalysis;
using static ChessLibrary.LayoutTypes;
using static ChessLibrary.MiscTypes;
using static ChessLibrary.PGNTypes;
using static ChessLibrary.PuzzleTypes;
using static ChessLibrary.Tournament;
using static ChessLibrary.TypesDef;
using static ChessLibrary.TypesDef.CoreTypes;

namespace WebGUI.Components.Pages.TournamentPages;

public partial class Tournaments
{
	Chess.Board board = new ChessLibrary.Chess.Board();
	List<string> validSizes = ["small", "medium", "large"];
	private int height = 100;
	private bool shouldCycle;
	private ThreadSafeBoardState boardState = new();
	private MoveAndFen WhiteMoveAndFen = MoveAndFen.FirstEntry;
	private MoveAndFen BlackMoveAndFen = MoveAndFen.FirstEntry;
	private PVboardDuo pvBoardDuo;
	private bool BestMoveWithPolicy = false;
	private bool blackLogLive = false;
	private bool whiteLogLive = false;
	private StreamingChessboard streamingBoard;	
	private bool showCompletePV = false;
	private string standingsTableHeight { get; set; }
	private IDialogReference dialogReference;
	private IDialogReference bracketDialogReference;
	private bool pendingCupDialog;
	private bool pendingSwissDialog;
	private bool pendingLadderDialog;
	private IDialogReference swissDialogReference;
	private IDialogReference ladderDialogReference;
	private IDialogReference resultDialogReference;
	private List<CrossTableEntry> table = new();
	private bool swapTables = true;
	private int autoCycleTimeInSec = 30;
	private bool showPVBoard = false;
	private MarkupString crosstableHtml { get; set; }
	// Ceilings, not sizes: .eb-scale on the elements turns each into a font-size, so the
	// description and the PV boxes follow the user's nudge like the tables do.
	/// The description's ceiling, after the measurement that keeps its bullets on one line.
	/// A property rather than a field: it was being rebuilt in three places that all had to
	/// remember the same format, and now the measurement is a fourth input.
	private string descriptionStyle =>
		"margin-left:10px; --eb-font-max:"
		+ ((layoutOptions.Fonts.TournamentDescFont > 0 ? layoutOptions.Fonts.TournamentDescFont : 14)
		   * descriptionFit.Value).ToString("0.##", System.Globalization.CultureInfo.InvariantCulture)
		+ "px;";

	private string pvStyleWhite = "--eb-font-max:16px;";
	private string pvStyleBlack = "--eb-font-max:16px;";
	private string swissRoundLabel = "";
	private List<string> MessagesFromCeres = new();
	private int TB = 0;
	private int r3 = 0;
	private int move50 = 0;
	private string reason = "";
	private int DrawP = 0;
	private string tournamentGamesHeader = "";
	private TypesDef.Tournament.Tournament tournament = TypesDef.Tournament.Tournament.Empty;
	private string tournamentLoadError = "";
	private InfoBannerInfo infoBannerInfo;
	private string currentOpeningInPlay = "Opening:";
	private string tournamentDesc = string.Empty;
	private LayoutOption layoutOptions = LayoutOption.Default;
	private int pairingTableHeight = 100;
	private string whiteTime = "00:00:00";
	private string blackTime = "00:00:00";
	private string whiteMoveTime = "00:00:00";
	private string blackMoveTime = "00:00:00";
	private string whitePlayer = "White player";
	private string blackPlayer = "Black player";
	private string blackWDL = ""; //$"[33.3 W | 33.3 D | 33.3 L]";
	private string whiteWDL = ""; //$"[33.3 W | 33.3 D | 33.3 L]";
	private string fen;
	private string blackPV = ""; //"PV: 1.e4 e5 2.Nf3 Nf6 3.Nxe5 d6  ";
	private string whitePV = ""; //"PV: 1.e4 e5 2.Nf3 Nf6 3.Nxe5 d6  ";
	private string whitePVda = "";
	private string whiteBefore = "";
	private string whiteRest = "";
	private string blackPVda = "";
	private string blackBefore = "";
	private string blackRest = "";
	private string blackLongPV = "";
	private string whiteLongPV = "";
	private bool whiteToMove = true;
	private TimeSpan whiteClock;
	private TimeSpan blackClock;
	private string moveTimer;
	private List<Result> results = new();
	private List<Pairing> pairings = new();
	private List<PlayerResult> scoreTable = new();
	private List<EngineStatus> engineStatus = new();
	private EngineStatus Engine1 = EngineStatus.Empty;
	private EngineStatus Engine2 = EngineStatus.Empty;
	private ChessLibrary.Tournament.Manager.Runner runner => TournamentSvc.CurrentRunner;
	// Feed mode: the page is driven by an external JSON feed (JsonFeedService) instead of the
	// internal engine runner. Same render path; the only differences are the update source and
	// that runner-only operations are skipped (see LiveFeedContract.md).
	private bool FeedMode => Nav.Uri.Contains("tournament-feed", StringComparison.OrdinalIgnoreCase);
	// Focused feed view (?game=ID): subscribe to the demuxed stream filtered to one gameId.
	private Action<string, TournamentTypes.Update> feedMultiHandler;
	// True only when THIS page started a replay (so a focused view doesn't stop the grid's replay).
	private bool startedReplay;
	// Read a query-string value from the current URL (dependency-free). Used for the dev
	// record/replay triggers: /tournament?record=PATH and /tournament-feed?replay=PATH[&speed=MS].
	private string GetQuery(string key)
	{
		var q = new Uri(Nav.Uri).Query;
		if (string.IsNullOrEmpty(q)) return null;
		foreach (var part in q.TrimStart('?').Split('&', StringSplitOptions.RemoveEmptyEntries))
		{
			var kv = part.Split('=', 2);
			if (kv.Length == 2 && Uri.UnescapeDataString(kv[0]).Equals(key, StringComparison.OrdinalIgnoreCase))
				return Uri.UnescapeDataString(kv[1]);
		}
		return null;
	}
	// Standings/crosstable: in feed mode compute from the accumulated results (no runner); in internal
	// mode delegate to the runner (unchanged behavior). Standings derive the roster from the results
	// themselves (the actual fed players) — NOT the local wwwroot tournament.json roster, which the
	// fed tournament's [JsonIgnore] Engines would otherwise be replaced by at StartOfTournament.
	private List<PlayerResult> PlayerResults(List<Result> res)
		=> FeedMode ? FeedStats.playerResultsFromResults(res) : runner.GetPlayerResults(res);
	private List<CrossTableEntry> Crosstable(List<Result> res)
		=> FeedMode ? FeedStats.smallCrosstable(tournament, res) : runner.GenerateStatsCrosstable(res);
	private List<CrossTableEntry> GauntletCrosstable(List<Result> res)
		=> FeedMode ? FeedStats.bigCrosstable(tournament, res) : runner.GetGauntletCrosstable(res);
	private EngineConfig WhiteEngineConfig = EngineConfig.Empty;
	private EngineConfig BlackEngineConfig = EngineConfig.Empty;
	private string moveHistory = string.Empty;
	private int moveNr = 0;
	private TypesDef.Tournament.StartOfTournamentInfo startTournyInfo = TypesDef.Tournament.StartOfTournamentInfo.Empty;
	private ElementReference evalChart;
	private ElementReference nodesChart;
	private ElementReference npsChart;
	private ElementReference epsChart;
	private ElementReference timeUsageChart;
	private ElementReference livePlotReference;
	private ElementReference liveQPlotReference;
	private IJSObjectReference chessModule;
	private DotNetObjectReference<Tournaments> dotNetRef;
	private LivePlot evalList;
	private LivePlot nodeList;
	private LivePlot npsList;
	private LivePlot epsList;
	private LivePlot timeUsageList;
	private SearchInfoPlot searchInfoPlot;
	private bool runWithLogLiveStats = false;
	private bool showNpsSpeedChart = true;
	private bool speedChartCycleEnabled = false;
	private bool whiteHasEps = false;
	private bool blackHasEps = false;
	private DateTimeOffset nextSpeedChartToggleUtc = DateTimeOffset.MinValue;
	private PeriodicTimer timer;
	private PeriodicTimer oneSecondTimer;
	private string openingMoves;
	private string whiteEngineLogo = "Img/EngineBattle.png";
	private string blackEngineLogo = "Img/EngineBattle.png";
	private string blackDev = "";
	private string whiteDev = "";
	private string speedDiff = "";
	private string gameResult = "";
	private ChessConfigurationService setting;
	HotKeysContext keyContext;
	//private readonly SemaphoreSlim _semaphore = new SemaphoreSlim(1, 1);
	private double tournamentHeight = 0;
	private double windowHeight = 0;
	private int moveListHeight = 100;
	private int totalNumberOfPairs = 0;
	private string roundNr = string.Empty;
	private int activeGameNr = 0;
	private List<string> openingSansForDeviation = new();
	private List<string> referenceSansForDeviation = new();

	private int CalcNumberOfChartsToShow()
	{
		int count = 0;
		if (runWithLogLiveStats)
			count += 2;
		if (ShowEval())
			count++;
		if (ShowTime())
			count++;
		if (ShowNodes())
			count++;
		if (ShowNPS())
			count++;
		return count;
	}

	public async Task Callback(string lanMove, double policy, double frac)
	{
		if (BestMoveWithPolicy)
		{
			await streamingBoard.UpdatePonderMoveWithPolicy(lanMove, policy, frac);
		}
	}

	private void SetPVStyle(LayoutOption layout)
	{
		pvStyleWhite = $"--eb-font-max:{layout.Fonts.PVLabelFont}px;";
		pvStyleBlack = $"--eb-font-max:{layout.Fonts.PVLabelFont}px;";
	}

	private bool ShowEval()
	{
		var showEval = tournament.LayoutOption.Charts.ShowEval;
		return runWithLogLiveStats ? showEval : true;
	}

	private bool ShowTime()
	{
		var showTime = tournament.LayoutOption.Charts.ShowTime;
		return runWithLogLiveStats ? showTime : true;
	}

	private bool ShowNodes()
	{
		var showNodes = tournament.LayoutOption.Charts.ShowNodes;
		return runWithLogLiveStats ? showNodes : true;
	}

	private bool ShowNPS()
	{
		var showNPS = tournament.LayoutOption.Charts.ShowNPS;
		return runWithLogLiveStats ? showNPS : true;
	}

	private bool ShowNpsSpeedChart()
	{
		if (!ShowNPS())
			return false;

		return !CanCycleSpeedCharts() || showNpsSpeedChart;
	}

	private bool ShowEpsSpeedChart()
	{
		if (!ShowNPS())
			return false;

		return CanCycleSpeedCharts() && !showNpsSpeedChart;
	}

	private bool CanCycleSpeedCharts() => whiteHasEps && blackHasEps;

	private int SpeedChartCycleSeconds => Math.Max(15, autoCycleTimeInSec);

	async Task OpenResultDialog()
	{
		CloseAllDialogs();
		var option = new DialogOptions() { MaxWidth = MaxWidth.Small, FullWidth = false, CloseButton = false, Position = DialogPosition.TopCenter };
		var title = $"Game Ended";
		var res = $"{infoBannerInfo.ResultTxt} ({reason})";
		var parameters = new DialogParameters
	  {
		{"Result", res }
	  };

		resultDialogReference = await DialogService.ShowAsync<Components.Layout.ExperimentalLayout.ResultDialog>(title, parameters, option);
	}

	void CloseResultDialog() => resultDialogReference?.Close();

	// The parallel runner has no adjudication channel reader, so "the current game" is
	// undefined and the request would be a silent no-op — surface that instead.
	bool AdjudicationBlocked()
	{
		if (!TournamentSvc.IsParallelRun)
			return false;
		Snackbar.Add("User adjudication is not available while games run in parallel.", Severity.Info);
		return true;
	}

	async Task WhiteWins()
	{
		if (AdjudicationBlocked()) return;
		reason = "White wins by user";
		gameResult = "1-0";
		runner?.AdjudicateGame(activeGameNr, "1-0");
		timer?.Dispose();
		oneSecondTimer?.Dispose();
		await Task.CompletedTask;
	}

	async Task BlackWins()
	{
		if (AdjudicationBlocked()) return;
		reason = "Black wins by user";
		gameResult = "0-1";
		runner?.AdjudicateGame(activeGameNr, "0-1");
		timer?.Dispose();
		oneSecondTimer?.Dispose();
		await Task.CompletedTask;
	}
	async Task DrawGame()
	{
		if (AdjudicationBlocked()) return;
		reason = "Draw by user";
		gameResult = "1/2-1/2";
		runner?.AdjudicateGame(activeGameNr, "1/2-1/2");
		timer?.Dispose();
		oneSecondTimer?.Dispose();
		await Task.CompletedTask;
	}

	async Task OpenDialog()
	{
		if (results.Count > 3000)
			return;

		CloseAllDialogs();

		var players = tournament?.EngineSetup?.Engines?.Count() ?? 0;
		var fontSize = layoutOptions.Fonts.CrossTableFont + 1;
		var crossTable = (IsGauntletMode || players == 2) ? GauntletCrosstable(results) : table;
		crossTable ??= new List<CrossTableEntry>();
		var tableCount = table?.Count ?? 0;
		var size = (IsGauntletMode || players == 2) ? tableCount - crossTable.Count : tableCount;
		var res = whitePlayer + " vs " + blackPlayer + ": " + gameResult;
		var options = size switch
		{
			> 5 => new DialogOptions() { MaxWidth = MaxWidth.ExtraExtraLarge, FullWidth = true, CloseButton = false },
			> 3 => new DialogOptions() { MaxWidth = MaxWidth.ExtraLarge, FullWidth = true, CloseButton = false },
			_ => new DialogOptions() { MaxWidth = MaxWidth.ExtraLarge, FullWidth = true, CloseButton = false }
		};
		var parameters = new DialogParameters
	  {
		{"CrossTable", crossTable },
		{"FontSize",  fontSize },
		{"Players",  players },
		{"GameResult", res},
		{"Gauntlet",  (IsGauntletMode || players == 2) }
	  };
		dialogReference = await DialogService.ShowAsync<Components.Layout.CrosstableLayout.CrosstableDialog>("", parameters, options);
	}

	async Task OpenSwissDialog(bool showActions = false)
	{
		CloseAllDialogs();
		await Task.Delay(1000);
		var options = new DialogOptions()
		{
			MaxWidth = MaxWidth.ExtraExtraLarge,
			FullWidth = true,
			CloseButton = false,
			Position = DialogPosition.TopCenter
		};

		var parameters = new DialogParameters
		{
			{ "StatePath", GetSwissStatePath() },
			{ "ShowActions", showActions },
			{ "SwissCompleted", false },
			{ "SwissInvalid", false },
			{ "FontSize", layoutOptions.Fonts.SwissOverviewFont }
		};
		swissDialogReference = await DialogService.ShowAsync<Components.Layout.TournamentLayout.SwissOverviewDialog>("", parameters, options);
	}

	async Task OpenCupBracketDialog()
	{
		CloseAllDialogs();
		await Task.Delay(1000);
		var options = new DialogOptions()
		{
			MaxWidth = MaxWidth.ExtraLarge,
			FullWidth = true,
			CloseButton = false,
			Position = DialogPosition.TopCenter
		};

		var parameters = new DialogParameters
		{
			{ "FontSize", layoutOptions.Fonts.CupBracketFont }
		};
		bracketDialogReference = await DialogService.ShowAsync<Components.Layout.TournamentLayout.CupBracketDialog>("", parameters, options);
	}

	async Task OpenLadderDialog()
	{
		CloseAllDialogs();
		await Task.Delay(1000);
		var options = new DialogOptions()
		{
			MaxWidth = MaxWidth.ExtraExtraLarge,
			FullWidth = true,
			CloseButton = false,
			Position = DialogPosition.TopCenter
		};

		var parameters = new DialogParameters
		{
			{ "StatePath", GetLadderStatePath() },
			{ "ShowActions", false },
			{ "LadderCompleted", false },
			{ "LadderInvalid", false },
			{ "FontSize", layoutOptions.Fonts.LadderOverviewFont }
		};
		ladderDialogReference = await DialogService.ShowAsync<Components.Layout.TournamentLayout.LadderResumeDialog>("", parameters, options);
	}

	void CloseDialog() => dialogReference?.Close();

	void CloseBetweenGamesDialogs()
	{
		CloseAllDialogs();
	}

	void CloseAllDialogs()
	{
		dialogReference?.Close();
		dialogReference = null;
		bracketDialogReference?.Close();
		bracketDialogReference = null;
		swissDialogReference?.Close();
		swissDialogReference = null;
		ladderDialogReference?.Close();
		ladderDialogReference = null;
		resultDialogReference?.Close();
		resultDialogReference = null;
	}

	private async Task UpdatePVBoards(string longPV, int depthToMove, int depthOpp, bool white, string newFen)
	{
		try
		{
			var moves = boardState.GetPVMoves(longPV, depthToMove, false).ToList();
			var moveFen = await boardState.PlayPVMoves(moves, newFen);
			if (white)
			{
				WhiteMoveAndFen = moveFen;
			}
			else
			{
				BlackMoveAndFen = moveFen;
			}
		}
		catch (Exception e)
		{
			logger.LogDebug($"Logging exception data: longPV: {longPV} depthToMove: {depthToMove} depthOpp: {depthOpp} white: {white} newFen: {newFen}");
			logger.LogError(e.Message);
		}
	}


	private int CountAgreements(string[] wArr, string[] bArr)
	{
		var shortest = Math.Min(wArr.Length, bArr.Length);
		int equalCount = 0;
		for (int i = 0; i < shortest; i++)
		{
			if (wArr[i].Equals(bArr[i]))
			{
				equalCount++;
			}

			else
				return equalCount;
		}
		return equalCount;
	}

	private async Task<int> CalcPVAgreement(bool whiteToPlay)
	{
		var newFen = fen;
		if (string.IsNullOrEmpty(whiteLongPV) || string.IsNullOrEmpty(blackLongPV))
		{
			if (string.IsNullOrEmpty(whiteLongPV) == false && whiteToMove)
			{
				whitePVda = string.Empty; whiteBefore = string.Empty;
				whiteRest = whitePV;
				if (showPVBoard)
					await UpdatePVBoards(whiteLongPV, 1, 0, whiteToPlay, newFen);
				return 0;
			}
			if (string.IsNullOrEmpty(blackLongPV) == false && whiteToMove == false)
			{
				blackPVda = String.Empty; blackBefore = string.Empty;
				blackRest = blackPV;
				if (showPVBoard)
					await UpdatePVBoards(blackLongPV, 1, 0, whiteToPlay, newFen);
				return 0;
			}
			if (showPVBoard)
			{
				if (whiteToPlay)
				{
					await UpdatePVBoards(whiteLongPV, 1, 0, whiteToPlay, newFen);
				}
				else
				{
					await UpdatePVBoards(blackLongPV, 1, 0, whiteToPlay, newFen);
				}
				return 0;
			}
		}

		var comparePV = whiteToPlay ? blackLongPV : whiteLongPV;
		var toMovePV = whiteToPlay ? whiteLongPV : blackLongPV;
		var toCompareArr = comparePV.Split(' ')[1..];
		var toMoveArr = toMovePV.Split(' ');
		int numberOfMovesAgreement = CountAgreements(toMoveArr, toCompareArr);
		var wArr = whitePV.Split(' ');
		var bArr = blackPV.Split(' ');
		var blackArr = bArr.Length > 1 ? bArr[1..] : Array.Empty<string>();
		var res = numberOfMovesAgreement;
		if (whiteToPlay)
		{
			var blackArrCompare = blackArr.Length > 1 ? blackArr[1..] : Array.Empty<string>();
			whiteBefore = String.Join(" ", wArr.Take(res));
			whitePVda = wArr.Length > res ? wArr[res] : "";
			whiteRest = String.Join(" ", wArr.Skip(res + 1));

			int blackTakeCount = Math.Min(bArr.Length, res + 2);
			blackBefore = String.Join(" ", bArr.Take(blackTakeCount));
			blackPVda = blackArrCompare.Length > res ? blackArrCompare[res] : "";
			blackRest = String.Join(" ", blackArrCompare.Skip(res + 1));
		}

		else
		{
			var whiteArr = wArr.Length > 1 ? wArr[1..] : Array.Empty<string>();
			res = CountAgreements(whiteArr, blackArr);
			int blackTakeCount = Math.Min(bArr.Length, res + 1);
			blackBefore = String.Join(" ", bArr.Take(blackTakeCount));
			blackPVda = blackArr.Length > res ? blackArr[res] : "";
			blackRest = String.Join(" ", blackArr.Skip(res + 1));

			int whiteTakeCount = Math.Min(wArr.Length, res + 1);
			whiteBefore = String.Join(" ", wArr.Take(whiteTakeCount));
			whitePVda = whiteArr.Length > res ? whiteArr[res] : "";
			whiteRest = String.Join(" ", whiteArr.Skip(res + 1));
		}

		if (showPVBoard)
		{
			var longPV = whiteToPlay ? whiteLongPV : blackLongPV;
			await UpdatePVBoards(longPV, numberOfMovesAgreement + 1, numberOfMovesAgreement + 2, whiteToPlay, newFen);
		}

		return numberOfMovesAgreement;
	}


	string Gauntlet => tournament == null ? "" : IsCupMode ?
				GetCupSummary() :
				IsSwissMode ?
				GetSwissSummary() :
				IsLadderMode ?
				$"Ladder, {tournament.EngineSetup.Engines.Count()} engines, {tournament.LadderOptions.GamePairsPerMatch} game pairs per match" :
				IsGauntletMode ?
				$"{tournament.Challengers}-Gauntlet ({(tournament.Opening.OpeningsTwice ? "double" : "single")}), {@tournament.Rounds} rounds, {totalNumberOfPairs} total games" :
				$"{tournament.EngineSetup.Engines.Count()}-player {(tournament.Opening.OpeningsTwice ? "double" : "single")} Round robin, {@tournament.Rounds} rounds, {totalNumberOfPairs} total games";

	// Parts for TournamentSummary. The mode summaries below stay as they are — the badge is
	// just the format, and the sentence is what used to be two of the four bullets.
	string ModeBadge => tournament == null ? "" :
		IsCupMode ? "Cup" :
		IsSwissMode ? "Swiss" :
		IsLadderMode ? "Ladder" :
		IsGauntletMode ? $"{tournament.Challengers}-Gauntlet" :
		$"{tournament.EngineSetup.Engines.Count()}-player RR";

	string PairingBadge => tournament == null ? "" : tournament.Opening.OpeningsTwice ? "Double" : "Single";

	string SetupLine
	{
		get
		{
			if (tournament == null) return "";
			var format =
				IsCupMode ? GetCupSummary() :
				IsSwissMode ? GetSwissSummary() :
				IsLadderMode ? $"{tournament.EngineSetup.Engines.Count()} engines, {tournament.LadderOptions.GamePairsPerMatch} game pairs per match" :
				$"{tournament.Rounds} rounds, {totalNumberOfPairs} games";
			var order = tournament.Opening.RandomOpenings ? "random" : "sequential";
			var twice = tournament.Opening.OpeningsTwice ? " ×2" : "";
			var openings = $"{tournament.GetOpeningFileName()}, {tournament.Opening.OpeningsPly} ply, {order}{twice}";
			return $"{format} · {openings}";
		}
	}

	// Two forms of the same rule: a token for the banner, which is a strip of short values,
	// and the sentence for its tooltip.
	string WinRuleToken => tournament == null ? "" :
		$"≥{tournament.Adjudication.WinOption.MinWinScore} ×{tournament.Adjudication.WinOption.WinMoveLength}";

	string WinRuleText => tournament == null ? "" :
		$"Win: evals ≥ {tournament.Adjudication.WinOption.MinWinScore} for {tournament.Adjudication.WinOption.WinMoveLength} moves";

	string DrawRuleToken => tournament == null ? "" :
		$"≤{tournament.Adjudication.DrawOption.MaxDrawScore} ×{tournament.Adjudication.DrawOption.DrawMoveLength} @{tournament.Adjudication.DrawOption.MinDrawMove}";

	string DrawRuleText => tournament == null ? "" :
		$"Draw: after {tournament.Adjudication.DrawOption.MinDrawMove} moves, evals ≤ {tournament.Adjudication.DrawOption.MaxDrawScore} for {tournament.Adjudication.DrawOption.DrawMoveLength} moves";

	bool IsGauntletMode => tournament != null && tournament.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase);
	bool IsCupMode => tournament != null && tournament.TournamentMode.Equals("Cup", StringComparison.OrdinalIgnoreCase);
	bool IsSwissMode => tournament != null && tournament.TournamentMode.Equals("Swiss", StringComparison.OrdinalIgnoreCase);
	bool IsLadderMode => tournament != null && tournament.TournamentMode.Equals("Ladder", StringComparison.OrdinalIgnoreCase);

	string GetOpening()
	{
		if (tournament != null)
		{
			if (IsCupMode)
			{
				var uniqueText = tournament.CupOptions.UniquePerMatchOnly ? "unique per match" : "global unique";
				var orderText = tournament.CupOptions.RandomOpenings ? "random" : "sequential";
				return
					$"Openings, {tournament.Opening.OpeningsPly}ply, {orderText}, {uniqueText}, {tournament.GetOpeningFileName()}";
			}
			var orderText2 = tournament.Opening.RandomOpenings ? "random" : "sequential";
			return
				$"Openings, {tournament.Opening.OpeningsPly}ply, {orderText2}, {(tournament.Opening.OpeningsTwice ? "twice" : "single")}, {tournament.GetOpeningFileName()}";
		}
		else
			return "";
	}

	string GetCupSummary()
	{
		if (tournament == null)
			return "";

		var players = tournament.EngineSetup.Engines.Count();
		var pairs = tournament.CupOptions.RoundPairIncrements;
		var pairsText = pairs != null && pairs.Any()
			? $"{string.Join(" / ", pairs.Select(p => p.ToString()))} pairs/round"
			: "1 pair/round";
		return $"{players}-player Cup, {pairsText}, {totalNumberOfPairs} total games";
	}

	string GetSwissSummary()
	{
		if (tournament == null)
			return "";

		var players = tournament.EngineSetup.Engines.Count();
		var rounds = tournament.SwissOptions.Rounds > 0 ? tournament.SwissOptions.Rounds : tournament.Rounds;
		var pairsText = $"{Math.Max(1, tournament.SwissOptions.GamesPerMatch / 2)} pairs/round";
		return $"{players}-player Swiss, {rounds} rounds, {pairsText}, {totalNumberOfPairs} total games";
	}

	void UpdateSwissRoundLabel()
	{
		if (!IsSwissMode || tournament == null)
		{
			swissRoundLabel = "";
			return;
		}

		var totalRounds = Math.Max(1, tournament.SwissOptions.Rounds > 0 ? tournament.SwissOptions.Rounds : tournament.Rounds);
		var statePath = GetSwissStatePath();
		if (string.IsNullOrWhiteSpace(statePath) || !File.Exists(statePath))
		{
			swissRoundLabel = $" (Round 1/{totalRounds})";
			return;
		}

		try
		{
			var json = File.ReadAllText(statePath);
			if (string.IsNullOrWhiteSpace(json))
			{
				swissRoundLabel = $" (Round 1/{totalRounds})";
				return;
			}

			var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = true };
			var state = JsonSerializer.Deserialize<SwissStateSnapshot>(json, options);
			if (state?.Rounds == null || state.Rounds.Count == 0)
			{
				swissRoundLabel = $" (Round 1/{totalRounds})";
				return;
			}

			var ordered = state.Rounds.OrderBy(r => r.RoundNumber).ToList();
			var openRound = ordered.FirstOrDefault(r => (r.Pairings ?? []).Any(p => p != null && !p.IsDecided));
			var currentRound = openRound?.RoundNumber ?? ordered.Last().RoundNumber;
			swissRoundLabel = $" (Round {currentRound}/{totalRounds})";
		}
		catch (JsonException)
		{
			swissRoundLabel = "";
		}
		catch (IOException)
		{
			swissRoundLabel = "";
		}
	}

	string CalcTournamentDuration()
	{
		var Tsec = startTournyInfo.TournamentDurationSec;
		var Gsec = startTournyInfo.GameDurationInSec;
		if (Tsec == TimeSpan.Zero)
		{
			return "";
		}

		return $"Tournament time: {Tsec}, game time: {Gsec}";
	}

	// Keeps the banner's text and its progress figures in step; they come from the same
	// clamped numbers, so the bar can never disagree with the label beside it.
	void ApplyGameProgress()
	{
		if (infoBannerInfo == null) return;
		infoBannerInfo.Game = GetDisplayedGameText();
		var total = tournament == null ? 0 : (tournament.TotalGames > 0 ? tournament.TotalGames : totalNumberOfPairs);
		infoBannerInfo.TotalGames = total;
		infoBannerInfo.GameNr = tournament == null ? 0 : Math.Min(tournament.CurrentGameNr, Math.Max(0, total));
		ApplyCarriedBannerFields();
	}

	// Held on the component, not only on the banner: the banner object is rebuilt on every
	// game start and on a settings change, and the count is only recomputed at the end of a
	// game. ApplyGameProgress puts it back so it does not blink to a dash in between.
	private int pairsCompleted;
	private int pairsIncomplete;
	private int pairsTotal;

	// Pairs come from the PGN, not from the result list: an unfinished pair is only visible
	// once both games of an opening are on disk. Scoped to the matchup that just finished,
	// to match H2H beside it — with no names given, the last game in the file decides, which
	// is what keeps the figure frozen on that matchup between games.
	void ApplyPairCount(System.Collections.Generic.List<PGNTypes.PgnGame> games, string white = null, string black = null)
	{
		if (games == null || games.Count == 0)
		{
			pairsCompleted = 0;
			pairsIncomplete = 0;
			pairsTotal = 0;
		}
		else
		{
			if (string.IsNullOrWhiteSpace(white) || string.IsNullOrWhiteSpace(black))
			{
				var last = games[games.Count - 1];
				white = last.GameMetaData.White;
				black = last.GameMetaData.Black;
			}
			var pairs = Statistics.Pentanomial.pairsForMatchup(games, white, black);
			pairsCompleted = pairs.Item1;
			pairsIncomplete = pairs.Item2;
			pairsTotal = pairs.Item3;
		}
		ApplyCarriedBannerFields();
	}

	// Fields the banner must not lose when it is rebuilt at every game start.
	void ApplyCarriedBannerFields()
	{
		if (infoBannerInfo == null) return;
		infoBannerInfo.Pairs = pairsCompleted;
		infoBannerInfo.IncompletePairs = pairsIncomplete;
		infoBannerInfo.PairsTotal = pairsTotal;
	}

	string GetDisplayedGameText()
	{
		if (tournament == null)
			return "";

		var totalGames = tournament.TotalGames > 0 ? tournament.TotalGames : totalNumberOfPairs;
		if (totalGames <= 0)
			return "";

		var currentGame = Math.Min(tournament.CurrentGameNr, totalGames);
		return currentGame <= 0 ? "" : $"{currentGame}/{totalGames}";
	}

	string GetWin()
	{
		if (tournament != null)
		{
			return
				$"Win: after start, evals >= {tournament.Adjudication.WinOption.MinWinScore} for {tournament.Adjudication.WinOption.WinMoveLength} moves";
		}
		else
			return "";
	}

	string GetDraw()
	{
		if (tournament != null)
		{
			return
				$"Draw: after {tournament.Adjudication.DrawOption.MinDrawMove} moves, evals <= {tournament.Adjudication.DrawOption.MaxDrawScore} for {tournament.Adjudication.DrawOption.DrawMoveLength} moves";
		}
		else
			return "";
	}

	private double CalcSharpness(WDL wdl)
	{
		var w = wdl.Win / 1000;
		var l = wdl.Loss / 1000;
		var winPart = Math.Log(1 / w - 1);
		var lossPart = Math.Log(1 / l - 1);
		var sum = winPart + lossPart;
		var factor = 2.0 / sum;
		//logger.LogInformation($"Win: {w} Loss: {l} LogWin: {winPart} LogLoss: {lossPart} Factor: {factor}");
		return factor;
	}

	private string GetWDLString(WDL wdl)
	{
		var msg = $"[{(wdl.Win / 1000):P1} W | {(wdl.Draw / 1000):P1} D | {(wdl.Loss / 1000):P1} L]";
		return msg;
	}

	// Eval bar beside the streaming board: latest eval from the engine on move
	// (already white-perspective — the parser negates for the black engine).
	private double? tournamentEvalCp;
	private int? tournamentEvalMate;

	private void SetEngineStatus(EngineStatus status)
	{
		if (status.PlayerName == Engine1.PlayerName)
		{
			Engine1 = status;
		}
		else
		{
			Engine2 = status;
		}

		switch (status.Eval)
		{
			case EvalType.CP cp: (tournamentEvalCp, tournamentEvalMate) = (cp.Info, null); break;
			case EvalType.Mate m: (tournamentEvalCp, tournamentEvalMate) = (null, m.Info); break;
		}

		if (status.EPS > 0)
		{
			if (status.PlayerName == Engine1.PlayerName)
				whiteHasEps = true;
			else if (status.PlayerName == Engine2.PlayerName)
				blackHasEps = true;

			if (!speedChartCycleEnabled && CanCycleSpeedCharts())
			{
				speedChartCycleEnabled = true;
				showNpsSpeedChart = true;
				nextSpeedChartToggleUtc = DateTimeOffset.UtcNow.AddSeconds(SpeedChartCycleSeconds);
				_ = InvokeAsync(StateHasChanged);
			}
		}
	}

	private void SetEnginePonderStatus(EnginePonderStatus status)
	{
		if (status.PlayerName == Engine1.PlayerName)
		{
			Engine1 = EngineStatus.Create(
				status.PlayerName,
				status.Eval,
				status.Nodes,
				status.NPS,
				status.Depth,
				status.SD,
				status.TBhits,
				status.WDL,
				Engine1.PV,
				Engine1.PVLongSAN,
				Engine1.MultiPV);
		}
		else
		{
			Engine2 = EngineStatus.Create(
				status.PlayerName,
				status.Eval,
				status.Nodes,
				status.NPS,
				status.Depth,
				status.SD,
				status.TBhits,
				status.WDL,
				Engine2.PV,
				Engine2.PVLongSAN,
				Engine2.MultiPV);
		}
	}

	private void SetSpeedFactor()
	{
		if (engineStatus.Count > 1)
		{
			var (fst, fstName) = (engineStatus[0].NPS, engineStatus[0].PlayerName);
			var (snd, sndName) = (engineStatus[1].NPS, engineStatus[1].PlayerName);

			if (fst > snd)
			{
				var speed = Math.Round(fst / snd, 0);
				speedDiff = speed.ToString("N0");
			}
			else
			{
				var speed = Math.Round(snd / fst, 0);
				speedDiff = speed.ToString("N0");
			}
		}
	}

	private EngineConfig GetPlayer(string name)
	{
		return tournament.EngineSetup.Engines.FirstOrDefault(e => e.Name == name);
	}

	private void SetGameParams(StartGameInfo info)
	{
		var b = info.BlackPlayer;
		var w = info.WhitePlayer;
		BlackEngineConfig = b;
		WhiteEngineConfig = w;
		Engine1.PlayerName = w.Name;
		Engine2.PlayerName = b.Name;
		ResetSpeedChartCycle();
		if (tournament == null && runner != null)
			tournament = runner.Tournament();
		tournamentDesc = tournament.MinSummary();
		blackLogLive = b.Options.Keys.Contains("LogLiveStats");
		whiteLogLive = w.Options.Keys.Contains("LogLiveStats");

		runWithLogLiveStats = whiteLogLive || blackLogLive;
		if (runWithLogLiveStats)
		{
			object resB, resW;
			bool btest = false, wtest = false;
			if (b.Options.TryGetValue("LogLiveStats", out resB))
			{
				var logLiveValue = Convert.ToBoolean(resB.ToString());
				if (logLiveValue)
					btest = true;
			}

			if (w.Options.TryGetValue("LogLiveStats", out resW))
			{
				var logLiveValue = Convert.ToBoolean(resW.ToString());
				if (logLiveValue)
					wtest = true;
			}

			if (tournament.TestOptions.ValueTest)
			{
				runWithLogLiveStats = false;
			}

			else if (wtest)
			{
				runWithLogLiveStats = true;
				if (tournament.VerboseLogging)
					logger.LogInformation("Live stats charting enabled — requires VerboseMoveStats = true in the engine def");
				searchInfoPlot = new SearchInfoPlot(chessModule, liveQPlotReference, livePlotReference, "Q-value", "", "", Callback);
			}

			else if (btest)
			{
				runWithLogLiveStats = true;
				if (tournament.VerboseLogging)
					logger.LogInformation("Live stats charting enabled — requires VerboseMoveStats = true in the engine def");
				searchInfoPlot = new SearchInfoPlot(chessModule, liveQPlotReference, livePlotReference, "Q-value", "", "", Callback);
			}

			else
			{
				runWithLogLiveStats = false;
			}
		}
	}

	private async void CancelTournament()
	{
		try
		{
			TournamentSvc.Cancel();
			timer?.Dispose();
			oneSecondTimer?.Dispose();
			await Notifier.NotifyFullScreenRequested(false);
			await Task.Delay(200);
			await InvokeAsync(OnBrowserResize);
			await chessModule.InvokeVoidAsync("triggerResizeEvent");
		}
		catch (Exception ex)
		{
			// async void, may run off the dispatcher (called fire-and-forget from Update):
			// an escaping exception (e.g. JS interop after the tab closed) kills the process.
			logger.LogError(ex, "Error cancelling tournament");
		}
	}

	private (bool ok, string errors) ValidateTournamentJson()
	{
		var result = ChessLibrary.Configuration.Validation.validateTournament(tournament);
		if (result is ChessLibrary.Configuration.Validation.ValidationResult.Errors errResult)
		{
			var msgs = errResult.Item;
			return (false, string.Join("\n", msgs));
		}
		return (true, "");
	}

	private async Task ValidateTournamentInput()
	{
		var (jsonOk, errors) = ValidateTournamentJson();
		if (!jsonOk)
		{
			var opt = new DialogOptions { MaxWidth = MaxWidth.Medium, Position = DialogPosition.TopCenter };
			var title = "Tournament validation failed!";
			var res = await DialogService.ShowAsync<Components.Layout.ExperimentalLayout.DialogOkCancel>(title, new DialogParameters { { "Result", errors } }, opt);
			return;
		}

		ChessLibrary.Tournament.Manager.loadTournament();
		var ok = await ChessLibrary.Tournament.TournamentUtils.validateEnginesInTournament(tournament);
		if (!ok)
		{
			var opt = new DialogOptions { MaxWidth = MaxWidth.Medium, Position = DialogPosition.TopCenter };
			var title = "Engine validation failed!";
			var msg = "One or more engines failed to start or did not pass UCI validation. Check that all engine paths are correct and engines respond to the UCI protocol.";
			var res = await DialogService.ShowAsync<Components.Layout.ExperimentalLayout.DialogOkCancel>(title, new DialogParameters { { "Result", msg } }, opt);
			return;
		}
	}

	private async void StartTimer()
	{
		moveTimer = string.Empty;
		var elapsedFromStart = TimeSpan.Zero;
		var start = Stopwatch.GetTimestamp();
		if (oneSecondTimer != null)
			return;
		if (timer != null)
			return;
		timer = new PeriodicTimer(TimeSpan.FromMilliseconds(100));
		oneSecondTimer = new PeriodicTimer(TimeSpan.FromMilliseconds(500));
		var limit = TimeSpan.FromSeconds(30);
		TimeSpan tOnly;
		var lastToPlay = whiteToMove;

		while (oneSecondTimer != null && await oneSecondTimer.WaitForNextTickAsync())
		{
			try
			{
				MaybeToggleSpeedChart();
				if (whiteToMove != lastToPlay)
				{
					start = Stopwatch.GetTimestamp();
					moveTimer = string.Empty;
					lastToPlay = whiteToMove;
				}
				else
				{
					var t = Stopwatch.GetElapsedTime(start);
					var elapsed = t;
					moveTimer = OneSecondMoveTimeFormatted(elapsed);

					if (whiteToMove)
					{
						var ticks = whiteClock.Ticks - t.Ticks;
						if (ticks < limit.Ticks)
						{
							//logger.LogInformation("White player is low on time");
							oneSecondTimer?.Dispose();
							oneSecondTimer = null;
						}
						whiteMoveTime = moveTimer;
						tOnly = new TimeSpan(Math.Max(0, ticks));
						whiteTime = TimeLeftFormatted(tOnly);
						await Notifier.OnNextTick(whiteToMove, whiteTime, whiteMoveTime);
					}

					else
					{
						var ticks = blackClock.Ticks - t.Ticks;
						if (ticks < limit.Ticks)
						{
							//logger.LogInformation("Black player is low on time");
							oneSecondTimer?.Dispose();
							oneSecondTimer = null;
						}
						blackMoveTime = moveTimer;
						tOnly = new TimeSpan(Math.Max(0, ticks));
						blackTime = TimeLeftFormatted(tOnly);
						await Notifier.OnNextTick(whiteToMove, blackTime, blackMoveTime);
					}

				}
			}

			catch (Exception e)
			{
				//silently ignoring the error here....
				logger.LogError(e.Message);
				Console.WriteLine(e.Message);
			}
		}

		while (timer != null && await timer.WaitForNextTickAsync())
		{
			try
			{
				MaybeToggleSpeedChart();
				if (whiteToMove != lastToPlay)
				{
					start = Stopwatch.GetTimestamp();
					moveTimer = string.Empty;
					lastToPlay = whiteToMove;
				}
				else
				{
					var t = Stopwatch.GetElapsedTime(start);
					var elapsed = t;
					moveTimer = MoveTimeFormatted(elapsed);

					if (whiteToMove)
					{
						var ticks = whiteClock.Ticks - t.Ticks;
						whiteMoveTime = moveTimer;
						tOnly = new TimeSpan(Math.Max(0, ticks));
						whiteTime = MoveTimeFormatted(tOnly);
						await Notifier.OnNextTick(whiteToMove, whiteTime, whiteMoveTime);
					}

					else
					{
						var ticks = blackClock.Ticks - t.Ticks;
						blackMoveTime = moveTimer;
						tOnly = new TimeSpan(Math.Max(0, ticks));
						blackTime = MoveTimeFormatted(tOnly);
						await Notifier.OnNextTick(whiteToMove, blackTime, blackMoveTime);
					}
				}
			}

			catch (Exception e)
			{
				//silently ignoring the error here....
				logger.LogError(e.Message);
				Console.WriteLine(e.Message);
			}
		}
	}

	// Bumped whenever a newer board state is applied (live BestMove, new StartOfGame). The
	// opening animation checks it so it can never paint stale opening positions over a game
	// that is already past the opening — which happens on feed catch-up (mid-join snapshot)
	// and with the parallel runner, whose pre-initialized engines start moving immediately
	// instead of waiting out the opening-animation delay like the sequential GUI runner.
	private int boardSyncGen = 0;

	private async Task PlayOpeningMoves(List<MoveAndFen> moves)
	{
		var gen = boardSyncGen;
		moveHistory = string.Empty;
		openingMoves = string.Empty;
		var isWhite = true;
		var startMove = moves.Count > 0 ? (moves.Count + 1) / 2 + 1 : 1;
		await evalList.ClearData(whitePlayer, blackPlayer, startMove);
		await nodeList.ClearData(whitePlayer, blackPlayer, startMove);
		await npsList.ClearData(whitePlayer, blackPlayer, startMove);
		await epsList.ClearData(whitePlayer, blackPlayer, startMove);
		await timeUsageList.ClearData(whitePlayer, blackPlayer, startMove);

		if (moves.Count != 0)
		{
			foreach (var move in moves)
			{
				// Always accumulate the opening move-list text; animate the board only while
				// no newer position has been applied. Once the generation changes, finish the
				// bookkeeping instantly with no delays and no board writes.
				CreateOpeningMoves(move);
				isWhite = !isWhite;
				if (boardSyncGen != gen)
					continue;
				await Task.Delay(tournament.MinMoveTimeInMS);
				await InvokeAsync(StateHasChanged);
				if (boardSyncGen != gen)
					continue;
				await streamingBoard.OnNotifyMoveAndFen(move);
				if (pvBoardDuo != null)
					await pvBoardDuo.SetPVMoveWithAnnotation(move, true);
			}
		}

		if (moves.Count > 0)
		{
			var move = moves.Last();
			if (boardSyncGen == gen)
				fen = move.FenAfterMove;
			await Notifier.UpdateOpeningDone(move);
		}
		await DoChartUpdates();
	}

	private void CreateOpeningMoves(MoveAndFen move)
	{
		if (move.Move.Color == "w")
		{
			moveNr++;
			openingMoves = $"{openingMoves} {moveNr}. {move.ShortSan}";
		}

		else
			openingMoves = $"{openingMoves} {move.ShortSan}";
	}

	private async void ResetGameState()
	{
		infoBannerInfo.ResultTxt = " * ";
		(tournamentEvalCp, tournamentEvalMate) = (null, null);
		move50 = 0;
		r3 = 0;
		blackLongPV = "";
		whiteLongPV = "";
		blackPV = string.Empty;
		whitePV = string.Empty;
		blackPVda = string.Empty;
		whitePVda = string.Empty;
		blackBefore = string.Empty;
		whiteBefore = string.Empty;
		blackRest = string.Empty;
		whiteRest = string.Empty;
		if (searchInfoPlot != null)
			await searchInfoPlot?.ResetPlot(true);
	}


	private void RunTournament()
	{
		var r = TournamentSvc.CreateRunner(logger, shutdownTokenProvider);
		TournamentSvc.Subscribe(Update);
		TournamentSvc.MarkRunning();
		tournament = r.Tournament();
		Task.Factory.StartNew(() =>
			{
				try
				{
					r.Run();
				}
				catch (Exception ex)
				{
					logger.LogError("Tournament failed to start/run - " + tournament.MinSummary());
					logger.LogError(ex.Message);
					throw;
				}
			});

		evalList = new LivePlot(chessModule, evalChart, whitePlayer, blackPlayer, "Eval in CP", "Centipawns");
		nodeList = new LivePlot(chessModule, nodesChart, whitePlayer, blackPlayer, "Nodes per move", "Total nodes");
		npsList = new LivePlot(chessModule, npsChart, whitePlayer, blackPlayer, "Speed (NPS)", "Nodes per sec");
		epsList = new LivePlot(chessModule, epsChart, whitePlayer, blackPlayer, "Speed (EPS)", "NN eval per sec");
		timeUsageList = new LivePlot(chessModule, timeUsageChart, whitePlayer, blackPlayer, "Time in sec", "Time (sec)");
		if (timer != null)
			timer.Dispose();
		if (oneSecondTimer != null)
			oneSecondTimer.Dispose();

		StateHasChanged();

		// Parallel runs render on the multi-board grid; this single-game page would only
		// flicker between concurrent games.
		if (TournamentSvc.IsParallelRun)
			Nav.NavigateTo("/tournament-grid");
	}

	private async Task StartTournamentFlow()
	{
		if (FeedMode)
			return;   // feed mode is driven externally; never start a local engine tournament
		await PrepareRun();
		if (!await ConfirmCupResumeOrNew())
			return;
		if (!await ConfirmSwissResumeOrNew())
			return;
		if (!await ConfirmLadderResumeOrNew())
			return;
		RunTournament();
	}

	private async Task<bool> ConfirmSwissResumeOrNew()
	{
		if (!IsSwissMode)
			return true;

		var statePath = GetSwissStatePath();
		if (string.IsNullOrWhiteSpace(statePath) || !File.Exists(statePath))
			return true;

		var configuredRounds = tournament?.EffectiveSwissRounds() ?? 0;
		var stateSummary = GetSwissStateSummary(statePath, configuredRounds);
		if (stateSummary == SwissStateSummary.Missing)
			return true;

		var options = new DialogOptions() { MaxWidth = MaxWidth.ExtraLarge, FullWidth = true, CloseButton = false, Position = DialogPosition.TopCenter };
		var parameters = new DialogParameters
		{
			{ "StatePath", statePath },
			{ "SwissCompleted", stateSummary == SwissStateSummary.Completed },
			{ "SwissInvalid", stateSummary == SwissStateSummary.Invalid },
			{ "ShowActions", true },
			{ "FontSize", layoutOptions.Fonts.SwissOverviewFont }
		};
		var dialog = await DialogService.ShowAsync<Components.Layout.TournamentLayout.SwissOverviewDialog>("", parameters, options);
		var result = await dialog.Result;
		if (result.Canceled)
			return false;

		var action = result.Data as string ?? "";
		if (action.Equals("new", StringComparison.OrdinalIgnoreCase))
		{
			BackupSwissStateFile(statePath);
			DeleteSwissStateFiles(statePath);
		}

		return true;
	}

	private string GetSwissStatePath()
	{
		var configured = tournament?.SwissOptions?.StatePath ?? "wwwroot/swiss_state.json";
		if (Path.IsPathRooted(configured))
			return configured;
		return Path.Combine(Environment.ContentRootPath, configured);
	}

	private static void DeleteSwissStateFiles(string statePath)
	{
		try
		{
			var dir = Path.GetDirectoryName(statePath);
			if (File.Exists(statePath))
				File.Delete(statePath);
			if (!string.IsNullOrWhiteSpace(dir) && Directory.Exists(dir))
			{
				var pattern = Path.GetFileName(statePath) + ".tmp*";
				foreach (var file in Directory.GetFiles(dir, pattern))
				{
					try
					{
						File.Delete(file);
					}
					catch (IOException)
					{
					}
				}
			}
		}
		catch (IOException)
		{
		}
	}

	private static void BackupSwissStateFile(string statePath)
	{
		try
		{
			if (!File.Exists(statePath))
				return;

			var dir = Path.GetDirectoryName(statePath);
			if (string.IsNullOrWhiteSpace(dir))
				return;

			var backupPath = Path.Combine(dir, $"{Path.GetFileName(statePath)}.bak");
			File.Copy(statePath, backupPath, true);
		}
		catch (IOException)
		{
		}
	}

	private static SwissStateSummary GetSwissStateSummary(string statePath, int configuredRounds)
	{
		try
		{
			if (string.IsNullOrWhiteSpace(statePath) || !File.Exists(statePath))
				return SwissStateSummary.Missing;

			var json = File.ReadAllText(statePath);
			if (string.IsNullOrWhiteSpace(json))
				return SwissStateSummary.Missing;

			var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = true };
			var state = JsonSerializer.Deserialize<SwissStateSnapshot>(json, options);
			if (state == null || state.Rounds == null || state.Rounds.Count == 0)
				return SwissStateSummary.Invalid;

			var hasOpen = state.Rounds.SelectMany(r => r.Pairings ?? []).Any(p => p != null && !p.IsDecided);
			if (hasOpen)
				return SwissStateSummary.InProgress;

			// All saved rounds are decided, but if the configured round count
			// exceeds the number of completed rounds, the tournament is resumable.
			if (configuredRounds > state.Rounds.Count)
				return SwissStateSummary.InProgress;

			return SwissStateSummary.Completed;
		}
		catch (JsonException)
		{
			return SwissStateSummary.Invalid;
		}
		catch (IOException)
		{
			return SwissStateSummary.Invalid;
		}
	}

	private enum SwissStateSummary
	{
		Missing,
		InProgress,
		Completed,
		Invalid
	}

	private sealed class SwissStateSnapshot
	{
		public List<SwissRoundSnapshot> Rounds { get; set; } = new();
	}

	private sealed class SwissRoundSnapshot
	{
		public int RoundNumber { get; set; }
		public List<SwissPairingSnapshot> Pairings { get; set; } = new();
	}

	private sealed class SwissPairingSnapshot
	{
		public bool IsDecided { get; set; }
	}

	private async Task<bool> ConfirmLadderResumeOrNew()
	{
		if (!IsLadderMode)
			return true;

		var statePath = GetLadderStatePath();
		if (string.IsNullOrWhiteSpace(statePath) || !File.Exists(statePath))
			return true;

		var stateSummary = GetLadderStateSummary(statePath);
		if (stateSummary == LadderStateSummary.Missing)
			return true;

		var options = new DialogOptions() { MaxWidth = MaxWidth.ExtraExtraLarge, FullWidth = true, CloseButton = false, Position = DialogPosition.TopCenter };
		var parameters = new DialogParameters
		{
			{ "StatePath", statePath },
			{ "ShowActions", true },
			{ "LadderCompleted", stateSummary == LadderStateSummary.Completed },
			{ "LadderInvalid", stateSummary == LadderStateSummary.Invalid },
			{ "FontSize", layoutOptions.Fonts.LadderOverviewFont }
		};
		var dialog = await DialogService.ShowAsync<Components.Layout.TournamentLayout.LadderResumeDialog>("", parameters, options);
		var result = await dialog.Result;
		if (result.Canceled)
			return false;

		var action = result.Data as string ?? "";
		if (action.Equals("new", StringComparison.OrdinalIgnoreCase))
		{
			BackupLadderStateFile(statePath);
			DeleteLadderStateFiles(statePath);
		}

		return true;
	}

	private string GetLadderStatePath()
	{
		var configured = tournament?.LadderOptions?.StatePath ?? "wwwroot/ladder_state.json";
		if (Path.IsPathRooted(configured))
			return configured;
		return Path.Combine(Environment.ContentRootPath, configured);
	}

	private static void DeleteLadderStateFiles(string statePath)
	{
		try
		{
			var dir = Path.GetDirectoryName(statePath);
			if (File.Exists(statePath))
				File.Delete(statePath);
			if (!string.IsNullOrWhiteSpace(dir) && Directory.Exists(dir))
			{
				var pattern = Path.GetFileName(statePath) + ".tmp*";
				foreach (var file in Directory.GetFiles(dir, pattern))
				{
					try
					{
						File.Delete(file);
					}
					catch (IOException)
					{
					}
				}
			}
		}
		catch (IOException)
		{
		}
	}

	private static void BackupLadderStateFile(string statePath)
	{
		try
		{
			if (!File.Exists(statePath))
				return;

			var dir = Path.GetDirectoryName(statePath);
			if (string.IsNullOrWhiteSpace(dir))
				return;

			var backupPath = Path.Combine(dir, $"{Path.GetFileName(statePath)}.bak");
			File.Copy(statePath, backupPath, true);
		}
		catch (IOException)
		{
		}
	}

	private static LadderStateSummary GetLadderStateSummary(string statePath)
	{
		try
		{
			if (string.IsNullOrWhiteSpace(statePath) || !File.Exists(statePath))
				return LadderStateSummary.Missing;

			var json = File.ReadAllText(statePath);
			if (string.IsNullOrWhiteSpace(json))
				return LadderStateSummary.Missing;

			var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = true };
			var state = JsonSerializer.Deserialize<LadderStateSnapshot>(json, options);
			if (state == null || state.SurvivingEngines == null)
				return LadderStateSummary.Invalid;

			if (state.SurvivingEngines.Count <= 1)
				return LadderStateSummary.Completed;

			return LadderStateSummary.InProgress;
		}
		catch (JsonException)
		{
			return LadderStateSummary.Invalid;
		}
		catch (IOException)
		{
			return LadderStateSummary.Invalid;
		}
	}

	private enum LadderStateSummary
	{
		Missing,
		InProgress,
		Completed,
		Invalid
	}

	private sealed class LadderStateSnapshot
	{
		public List<string> SurvivingEngines { get; set; } = new();
		public List<string> EliminatedEngines { get; set; } = new();
		public List<LadderMatchSnapshot> Matches { get; set; } = new();
	}

	private sealed class LadderMatchSnapshot
	{
		public bool IsDecided { get; set; }
	}

	private async Task<bool> ConfirmCupResumeOrNew()
	{
		ChessLibrary.Tournament.Manager.setCupResumeRequested(false);
		ChessLibrary.Tournament.Manager.setCupBracketPathOverride(null);
		if (!IsCupMode)
			return true;

		var bracketPath = GetCupBracketPath();
		ChessLibrary.Tournament.Manager.setCupBracketPathOverride(bracketPath);
		if (string.IsNullOrWhiteSpace(bracketPath) || !File.Exists(bracketPath))
			return true;

		var bracketState = GetCupBracketSummary(bracketPath);
		if (bracketState == CupBracketSummary.Missing)
			return true;

		var options = new DialogOptions() { MaxWidth = MaxWidth.ExtraLarge, FullWidth = true, CloseButton = false, Position = DialogPosition.TopCenter };
		var parameters = new DialogParameters
		{
			{ "BracketPath", bracketPath },
			{ "CupCompleted", bracketState == CupBracketSummary.Completed },
			{ "CupInvalid", bracketState == CupBracketSummary.Invalid },
			{ "FontSize", layoutOptions.Fonts.CupBracketFont  }
		};
		var dialog = await DialogService.ShowAsync<Components.Layout.TournamentLayout.CupResumeDialog>("", parameters, options);
		var result = await dialog.Result;
		if (result.Canceled)
			return false;

		var action = result.Data as string ?? "";
		if (action.Equals("new", StringComparison.OrdinalIgnoreCase))
		{
			ChessLibrary.Tournament.Manager.setCupResumeRequested(false);
			BackupCupBracketFile(bracketPath);
			DeleteCupBracketFiles(bracketPath);
		}
		else if (action.Equals("resume", StringComparison.OrdinalIgnoreCase))
		{
			ChessLibrary.Tournament.Manager.setCupResumeRequested(true);
		}
		else
		{
			ChessLibrary.Tournament.Manager.setCupResumeRequested(false);
		}

		return true;
	}

	private string GetCupBracketPath()
	{
		var configured = tournament?.CupOptions?.BracketPath ?? "wwwroot/cup_bracket.json";
		if (Path.IsPathRooted(configured))
			return configured;
		return Path.Combine(Environment.ContentRootPath, configured);
	}

	private static void DeleteCupBracketFiles(string bracketPath)
	{
		try
		{
			var dir = Path.GetDirectoryName(bracketPath);
			if (File.Exists(bracketPath))
				File.Delete(bracketPath);
			if (!string.IsNullOrWhiteSpace(dir) && Directory.Exists(dir))
			{
				var pattern = Path.GetFileName(bracketPath) + ".tmp*";
				foreach (var file in Directory.GetFiles(dir, pattern))
				{
					try
					{
						File.Delete(file);
					}
					catch (IOException)
					{
					}
				}
			}
		}
		catch (IOException)
		{
		}
	}

	private static void BackupCupBracketFile(string bracketPath)
	{
		try
		{
			if (!File.Exists(bracketPath))
				return;

			var backupPath = bracketPath + ".bak";
			File.Copy(bracketPath, backupPath, true);
		}
		catch (IOException)
		{
		}
	}

	private static CupBracketSummary GetCupBracketSummary(string bracketPath)
	{
		try
		{
			using var stream = new FileStream(bracketPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite | FileShare.Delete);
			using var reader = new StreamReader(stream);
			var json = NormalizeBracketJson(reader.ReadToEnd());
			if (string.IsNullOrWhiteSpace(json))
				return CupBracketSummary.Invalid;

			var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = true, AllowTrailingCommas = true };
			var bracket = JsonSerializer.Deserialize<CupBracketState>(json, options);
			if (bracket?.Rounds is null)
				return CupBracketSummary.Invalid;

			var hasOpen = bracket.Rounds.Any(r => r.Matches?.Any(m => !m.IsDecided) == true);
			return hasOpen ? CupBracketSummary.InProgress : CupBracketSummary.Completed;
		}
		catch (IOException)
		{
			return CupBracketSummary.Invalid;
		}
		catch (JsonException)
		{
			return CupBracketSummary.Invalid;
		}
	}

	private static string NormalizeBracketJson(string json)
	{
		if (string.IsNullOrWhiteSpace(json))
			return json;

		var trimmed = json.TrimStart();
		if (trimmed.StartsWith("\"", StringComparison.Ordinal) && json.TrimEnd().EndsWith("}", StringComparison.Ordinal))
			return "{" + json;

		return json;
	}

	private void ResetSpeedChartCycle()
	{
		speedChartCycleEnabled = false;
		showNpsSpeedChart = true;
		whiteHasEps = false;
		blackHasEps = false;
		nextSpeedChartToggleUtc = DateTimeOffset.MinValue;
	}

	private void MaybeToggleSpeedChart()
	{
		if (!CanCycleSpeedCharts())
		{
			showNpsSpeedChart = true;
			speedChartCycleEnabled = false;
			return;
		}

		if (!speedChartCycleEnabled)
		{
			speedChartCycleEnabled = true;
			nextSpeedChartToggleUtc = DateTimeOffset.UtcNow.AddSeconds(SpeedChartCycleSeconds);
			return;
		}

		if (DateTimeOffset.UtcNow < nextSpeedChartToggleUtc)
			return;

		showNpsSpeedChart = !showNpsSpeedChart;
		nextSpeedChartToggleUtc = DateTimeOffset.UtcNow.AddSeconds(SpeedChartCycleSeconds);
		_ = InvokeAsync(async () =>
		{
			StateHasChanged();
			await Task.Delay(50);
			if (chessModule is not null)
			{
				var chart = showNpsSpeedChart ? npsChart : epsChart;
				await chessModule.InvokeVoidAsync("resizePlot", chart);
			}
		});
	}


	// ── Main board sizing ────────────────────────────────────────────────────────
	// Zero (the default) leaves the three-column grid exactly as it always was; the
	// classes and the width variable below only take effect once a size is chosen, and
	// the board's "auto" button puts it back.
	private int TournamentBoardSize => SettingsService.Settings.TournamentBoardSizePx;
	private bool BoardSizePinned => TournamentBoardSize > 0;
	private string SideColumnClass => BoardSizePinned ? "tp-col-flex" : "";
	private string BoardColumnClass => BoardSizePinned ? "tp-col-board" : "";

	// Column width = board + eval bar and its gap + the grid item's own padding. Pinning
	// the width rather than letting the column size to content matters: the PV lines under
	// the board are long single lines, and their max-content would blow the column open.
	private string BoardColumnStyle =>
		BoardSizePinned
			? $"--tp-board-col:{TournamentBoardSize + (SettingsService.Settings.ShowEvalBar ? 22 : 0) + 24}px"
			: null;

	// The columns beside the board changed width: heights must be re-measured, and Plotly
	// has to be told its containers moved — the charts do not reflow on their own. Hidden
	// charts are skipped: resizing one measures a zero-size container and breaks it.
	private async Task OnBoardSizeChanged()
	{
		// Re-render first: the column width lives on this component, and measuring before
		// it has been applied would size the move list against the old grid.
		await InvokeAsync(StateHasChanged);
		await OnBrowserResize();
		if (chessModule is null) return;
		await Task.Delay(50);
		var visible = new List<ElementReference>();
		if (runWithLogLiveStats) { visible.Add(livePlotReference); visible.Add(liveQPlotReference); }
		if (ShowEval()) visible.Add(evalChart);
		if (ShowTime()) visible.Add(timeUsageChart);
		if (ShowNpsSpeedChart()) visible.Add(npsChart);
		if (ShowEpsSpeedChart()) visible.Add(epsChart);
		if (ShowNodes()) visible.Add(nodesChart);
		foreach (var chart in visible)
		{
			try { await chessModule.InvokeVoidAsync("resizePlot", chart); }
			catch (Exception ex) { logger.LogDebug("Chart resize after board resize failed: {Message}", ex.Message); }
		}
	}

	private async Task OnSettingAdded(ChessConfigurationService config)
	{
		setting = config;
		await Task.CompletedTask;
	}

	private void ValidatePVSize()
	{
		if (validSizes.Contains(layoutOptions.Sizes.PVboardSize) == false)
		{
			//write all available sizes to the log
			string logMsg = "Available PV board sizes are: ";
			foreach (var size in validSizes)
				logMsg += size + ", ";
			logger.LogInformation("Invalid PV board size: " + layoutOptions.Sizes.PVboardSize + " \n" + logMsg);
		}
	}

	private async Task PrepareRun()
	{
		await Notifier.NotifyFullScreenRequested(true);
		await UpdateState();
	}

	protected async override void OnAfterRender(bool firstRender)
	{
		if (firstRender)
		{
			if (!FeedMode && TournamentSvc.IsRunning && TournamentSvc.IsParallelRun)
			{
				// Parallel run: the internal (untagged) stream interleaves every board, so this
				// page would flip between concurrent games. Watch one board big instead — the
				// same page in focused feed mode, driven by that board's tagged stream. forceLoad
				// because both routes map to this component and this instance's first-render
				// setup is already consumed.
				Nav.NavigateTo("/tournament-feed?game=1", forceLoad: true);
				return;
			}
			chessModule = await JsInteropService.ImportModuleAsync(JS);
			dotNetRef = DotNetObjectReference.Create(this);
			await chessModule.InvokeVoidAsync("registerResizeEvent", dotNetRef);
			Notifier.IsFullScreenRequested += OnFullScreenRequested;

			// The size controls live in the bottom-right corner and only appear when the
			// pointer comes for them. The screen key has to come from the browser, so the
			// figure on the control is the shared default for one frame.
			await chessModule.InvokeVoidAsync("watchCorner", "fontscale", ".eb-fontscale", 70);
			fontScaleScreenKey = await JsInteropService.GetScreenBucketAsync(JS);
			fontScalePct = (int)Math.Round(SettingsService.Settings.FontScaleFor(fontScaleScreenKey) * 100);
			chartScalePct = (int)Math.Round(SettingsService.Settings.ChartScaleFor(fontScaleScreenKey) * 100);
			pvBoardChoice = SettingsService.Settings.TournamentPvBoard ?? "";
			ApplyPvBoardMode();
			// Nothing else schedules the frame that shows these, and without it the control
			// reads 100% and the wrong PV button is lit until some unrelated event re-renders.
			await InvokeAsync(StateHasChanged);

			keyContext = HotKeys.CreateContext()
			.Add(ModKey.Ctrl, Key.r, (Func<Task>)(async () => { await StartTournamentFlow(); }), "Run tournament")
			.Add(ModKey.Ctrl, Key.c, (() => CancelTournament()), "Cancel tournament")
			.Add(ModKey.Ctrl, Key.v, (Func<Task>)(async () => { await ValidateTournamentInput(); }), "Validate tournament input")
			.Add(ModKey.Ctrl, Key.u, (Func<Task>)(async () => { await UpdateState(); }), "Update state")
			.Add(ModKey.Ctrl, Key.p, (Func<Task>)(async () => { await OpenQuickDialog(); }), "Open tournament view")
			.Add(ModCode.Ctrl | ModCode.Alt, Code.W, (Func<Task>)(async () => { await WhiteWins(); }), "White wins")
			.Add(ModCode.Ctrl | ModCode.Alt, Code.B, (Func<Task>)(async () => { await BlackWins(); }), "Black wins")
			.Add(ModCode.Ctrl | ModCode.Alt, Code.D, (Func<Task>)(async () => { await DrawGame(); }), "Draw")
			.Add(ModCode.Ctrl | ModCode.Alt, Code.Minus, (() => NudgeFontScale(-5)), "Smaller table text")
			.Add(ModCode.Ctrl | ModCode.Alt, Code.Equal, (() => NudgeFontScale(5)), "Larger table text")
			.Add(ModCode.Ctrl | ModCode.Alt, Code.Num0, (() => ResetToConfiguredLayout()), "Text and PV boards back to tournament.json");
			//.Add(ModKey.Ctrl, Key.s, (() => SwapBothTables()), "Swap tables")
			//.Add(ModKey.Ctrl, Key.b, (() => BenchmarkConfig()), "Create benchmark config")
			//.Add(ModKey.Ctrl, Key.l, (() => OpenResultDialog()), "Show result");
			if (!FeedMode)
			{
				// Dev/test: /tournament?record=PATH tees the internal Update stream to an NDJSON file.
				var recordPath = GetQuery("record");
				if (!string.IsNullOrEmpty(recordPath))
					TournamentSvc.StartRecording(recordPath);
			}
			if (TournamentSvc.IsRunning && runner != null && !FeedMode)
			{
				// Reconnect to a running tournament (internal untagged stream — local page only;
				// a feed view during a local run must use the tagged JsonFeedService stream below,
				// otherwise a focused ?game view would show every board interleaved)
				tournament = runner.Tournament();
				results = runner.GetResults();
				scoreTable = runner.GetPlayerResults(results);
				table = runner.GenerateStatsCrosstable(results);
				pairings = runner.GetLastestPairings();
				infoBannerInfo = new InfoBannerInfo(tournament);
				// Re-create charts so live updates don't hit null
				evalList = new LivePlot(chessModule, evalChart, whitePlayer, blackPlayer, "Eval in CP", "Centipawns");
				nodeList = new LivePlot(chessModule, nodesChart, whitePlayer, blackPlayer, "Nodes per move", "Total nodes");
				npsList = new LivePlot(chessModule, npsChart, whitePlayer, blackPlayer, "Speed (NPS)", "Nodes per sec");
				epsList = new LivePlot(chessModule, epsChart, whitePlayer, blackPlayer, "Speed (EPS)", "NN eval per sec");
				timeUsageList = new LivePlot(chessModule, timeUsageChart, whitePlayer, blackPlayer, "Time in sec", "Time (sec)");
				// Subscribe last — after all state is ready for incoming updates
				TournamentSvc.Subscribe(Update);
			}
			else if (FeedMode)
			{
				// Feed mode: driven by an external JSON feed. Load the local tournament.json only for
				// layout/UI defaults; the actual tournament + games arrive via JsonFeedService events.
				try
				{
					var r = TournamentSvc.GetConfigRunner(logger);
					tournament = r.Tournament();
					if (tournament == null || tournament.EngineSetup?.Engines == null || tournament.EngineSetup.Engines.Length == 0)
					{
						// Feed mode does NOT require a local tournament.json — the real tournament (engines,
						// layout, time control) arrives via the feed's StartOfTournament event, which replaces
						// `tournament`. Fall back to defaults so the view loads even when the file is absent or
						// has no engines (an external runner / Ceres broadcast supplies everything).
						tournament = TypesDef.Tournament.Tournament.Empty;
					}
					results = new List<Result>();
					scoreTable = new List<PlayerResult>();
					table = new List<CrossTableEntry>();
					// Charts must exist before incoming BestMove events
					evalList = new LivePlot(chessModule, evalChart, whitePlayer, blackPlayer, "Eval in CP", "Centipawns");
					nodeList = new LivePlot(chessModule, nodesChart, whitePlayer, blackPlayer, "Nodes per move", "Total nodes");
					npsList = new LivePlot(chessModule, npsChart, whitePlayer, blackPlayer, "Speed (NPS)", "Nodes per sec");
					epsList = new LivePlot(chessModule, epsChart, whitePlayer, blackPlayer, "Speed (EPS)", "NN eval per sec");
					timeUsageList = new LivePlot(chessModule, timeUsageChart, whitePlayer, blackPlayer, "Time in sec", "Time (sec)");
					// Subscribe last — after all state is ready for incoming feed events.
					// ?game=ID focuses one game (from the grid): use the demuxed stream filtered to that
					// gameId (plus global events, gameId ""). Otherwise take the whole single-game stream.
					var gameFilter = GetQuery("game");
					if (!string.IsNullOrEmpty(gameFilter))
					{
						feedMultiHandler = (gid, upd) =>
						{
							// Live events arrive on the feed's background (bridge/replay) thread. Marshal
							// Update onto this component's dispatcher so its render + board/chart JS interop
							// run in the right context. (The catch-up snapshot already runs on the dispatcher
							// during init — which is why it rendered the current position but live deltas did
							// not until this wrap was added.)
							if (string.IsNullOrEmpty(gid) || gid == gameFilter)
								_ = InvokeAsync(() => Update(upd));
						};
						JsonFeedSvc.SubscribeMulti(feedMultiHandler);
					}
					else
					{
						JsonFeedSvc.Subscribe(Update);
					}
					// Dev/test: /tournament-feed?replay=PATH[&speed=MS] replays a recorded NDJSON file.
					var replayPath = GetQuery("replay");
					if (!string.IsNullOrEmpty(replayPath))
					{
						var speedMs = int.TryParse(GetQuery("speed"), out var ms) ? ms : 0;
						startedReplay = true;
						_ = Replayer.ReplayFileAsync(replayPath, speedMs);
					}
				}
				catch (Exception ex)
				{
					tournamentLoadError = ex.Message;
					logger.LogError(ex, "Failed to initialize feed mode");
					StateHasChanged();
					return;
				}
			}
			else
			{
				// Normal init — create config-reading runner
				try
				{
					var r = TournamentSvc.GetConfigRunner(logger);
					tournament = r.Tournament();
					if (tournament == null || tournament.EngineSetup?.Engines == null || tournament.EngineSetup.Engines.Length == 0)
					{
						var loadErr = ChessLibrary.Tournament.Manager.lastLoadError;
						tournamentLoadError = !string.IsNullOrEmpty(loadErr)
							? loadErr
							: "tournament.json could not be loaded. Check that all engine definition files exist and paths are correct.";
						StateHasChanged();
						return;
					}
					results = r.GetResults();
					scoreTable = r.GetPlayerResults(results);
					table = r.GenerateStatsCrosstable(results);
				}
				catch (Exception ex)
				{
					tournamentLoadError = ex.Message;
					logger.LogError(ex, "Failed to load tournament configuration");
					StateHasChanged();
					return;
				}
			}
			layoutOptions = tournament.LayoutOption;
			SetPVStyle(layoutOptions);
			BestMoveWithPolicy = layoutOptions.BestMoveWithPolicy;
			if (pairingTableHeight > 0)
			{
				ApplyPvBoardMode();
				autoCycleTimeInSec = layoutOptions.AutoCycleTimeInSec;
			}
			ValidatePVSize();
			var headToHead = infoBannerInfo != null ? infoBannerInfo.HeadToHead : string.Empty;
			infoBannerInfo = new InfoBannerInfo(tournament);
			infoBannerInfo.HeadToHead = headToHead;
			windowHeight = await chessModule.InvokeAsync<double>("getWindowHeight") - 10;
			tournamentHeight = await chessModule.InvokeAsync<double>("calculateHeightByClassName", "main-screen");
			var cycleT = await chessModule.InvokeAsync<double>("calculateHeightByElementId", "cycleTable");
			var tournamentHeightWithouthCycleTable = tournamentHeight - cycleT;

			// Move-list sizing happens in the OnBrowserResize() call at the end of this block,
			// measured rather than predicted.
			if (windowHeight < tournamentHeightWithouthCycleTable)
			{
				pairingTableHeight = 0;
			}

			if (windowHeight > tournamentHeightWithouthCycleTable)
			{
				var diff = Math.Abs(tournamentHeightWithouthCycleTable - windowHeight);
				pairingTableHeight = Math.Max(5, (int)(diff));
			}
			StateHasChanged();
			await OnBrowserResize();
		}

		base.OnAfterRender(firstRender);
	}

	private void SwapBothTables()
	{
		swapTables = !swapTables;
		StateHasChanged();
	}

	private void BenchmarkConfig()
	{
		var engines = tournament.EngineSetup.Engines.Where(e => e.Name.ToLower().Contains("lc0"));
		foreach (var config in engines)
		{
			var res = EngineProtocol.Engine.createLC0BenchmarkString(config);
			var msg = $"{config.Name}\n{res}";
			logger.LogInformation(msg);
		}
	}

	// The fences make the console table pasteable into Discord; in the log they only leave
	// blank lines behind. Collapse those runs, and close with one so the next entry has air.
	private static readonly System.Text.RegularExpressions.Regex BlankRun =
		new(@"(\r?\n){3,}", System.Text.RegularExpressions.RegexOptions.Compiled);

	private static string StripFences(string table) =>
		BlankRun.Replace(table.Replace("```", ""), "\n\n").Trim() + System.Environment.NewLine;

	private string GetStyleForResult(string result)
	{
		return result switch
		{
			"1" => "color: green;",
			"0" => "color: red;",
			"1/2" => "color: white;",  // or just return an empty string if you want the default style
			_ => ""
		};
	}

	private async Task OnFullScreenRequested(bool isFullScreen)
	{
		await OnBrowserResize();
	}

	protected async Task UpdateState()
	{
		var game = tournament.CurrentGameNr;
		var round = tournament.Rounds;
		var total = tournament.TotalGames;
		var tempTourny = ChessLibrary.Tournament.Manager.loadTournament();
		tournament.LayoutOption = tempTourny.LayoutOption;
		tournament.CurrentGameNr = game;
		tournament.Rounds = round;
		tournament.TotalGames = totalNumberOfPairs;
		layoutOptions = tournament.LayoutOption;
		ValidatePVSize();
		BestMoveWithPolicy = layoutOptions.BestMoveWithPolicy;
		ApplyPvBoardMode();
		autoCycleTimeInSec = layoutOptions.AutoCycleTimeInSec;
		SetPVStyle(layoutOptions);
		var bannerTime = infoBannerInfo.TimeLeftTxt;
		var bannerEnd = infoBannerInfo.TournamentEndsTxt;
		var headToHead = infoBannerInfo != null ? infoBannerInfo.HeadToHead : string.Empty;
		infoBannerInfo = new InfoBannerInfo(tournament);
		ApplyGameProgress();
		infoBannerInfo.TimeLeftTxt = bannerTime;
		infoBannerInfo.TournamentEndsTxt = bannerEnd;
		infoBannerInfo.Round = roundNr;
		infoBannerInfo.HeadToHead = headToHead;
		await InvokeAsync(StateHasChanged);
		await Task.Delay(300);
		await chessModule.InvokeVoidAsync("triggerResizeEvent");
		await OnBrowserResize();
	}

    public async ValueTask DisposeAsync()
    {
        // Handler-checked unsubscribe: the services hold a single global subscriber
        // slot, and a second tab's dispose used to null out the slot the FIRST tab
        // still owned — freezing it while the tournament ran on headless.
        TournamentSvc.Unsubscribe(Update);
        TournamentSvc.StopRecording();
        if (startedReplay) Replayer.Stop();
        JsonFeedSvc.Unsubscribe(Update);
        if (feedMultiHandler != null) JsonFeedSvc.UnsubscribeMulti(feedMultiHandler);
        Notifier.SettingAdded -= OnSettingAdded;
        Notifier.IsFullScreenRequested -= OnFullScreenRequested;
        try
        {
            if (chessModule != null) await chessModule.InvokeVoidAsync("unregisterResizeEvent");
        }
        catch (Microsoft.JSInterop.JSDisconnectedException) { }
        catch (ObjectDisposedException) { }
        if (keyContext != null) await keyContext.DisposeAsync();
        dotNetRef?.Dispose();
		if (timer != null)
			timer.Dispose();
        if (oneSecondTimer != null)
            oneSecondTimer.Dispose();
    }

    private sealed class CupBracketState
    {
        public List<CupRoundState> Rounds { get; set; } = new();
    }

    private sealed class CupRoundState
    {
        public List<CupMatchState> Matches { get; set; } = new();
    }

    private sealed class CupMatchState
    {
        public bool IsDecided { get; set; }
    }

    private enum CupBracketSummary
    {
        Missing,
        InProgress,
        Completed,
        Invalid
    }
}
