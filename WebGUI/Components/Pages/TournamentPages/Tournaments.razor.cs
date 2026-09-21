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
	private EngineStatsModern engineStats;
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
		+ (FontCeiling(FontKey.Description) * descriptionFit.Value)
			.ToString("0.##", System.Globalization.CultureInfo.InvariantCulture)
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
	/// The file is fine and names no engines: show the way in, not an error.
	private bool firstRun;
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
	/// The one clock ticker for the game on screen. Its PERIOD moves - 500ms normally, 100ms once
	/// the side to move is inside the last 30 seconds - rather than there being two timers and two
	/// loops, which is what this replaced: that pair never went back to 500ms once either side had
	/// dipped under 30 seconds, and it put BOTH sides on ten ticks a second for the rest of the game.
	private PeriodicTimer clockTimer;

	/// Stopwatch timestamp of the moment the side to move changed. Written where the move ARRIVES
	/// (Tournaments.LiveUpdates.cs), read by the ticker - so the move clock starts when the move
	/// does, not up to one tick later when the loop happened to notice the side had flipped.
	private long moveStartedAt;

	private void StopClock()
	{
		clockTimer?.Dispose();
		clockTimer = null;
	}

	/// The move clock starts now. Called where a move arrives and where a game starts, BEFORE the
	/// side and its clock are written - see the comment at the BestMove site in LiveUpdates.
	private void RestartMoveClock() =>
		System.Threading.Volatile.Write(ref moveStartedAt, Stopwatch.GetTimestamp());
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

	private void SetPVStyle()
	{
		pvStyleWhite = $"--eb-font-max:{FontCeiling(FontKey.Pv)}px;";
		pvStyleBlack = $"--eb-font-max:{FontCeiling(FontKey.Pv)}px;";
	}

	private bool ShowEval()
	{
		var showEval = layoutOptions.Charts.ShowEval;
		return runWithLogLiveStats ? showEval : true;
	}

	private bool ShowTime()
	{
		var showTime = layoutOptions.Charts.ShowTime;
		return runWithLogLiveStats ? showTime : true;
	}

	private bool ShowNodes()
	{
		var showNodes = layoutOptions.Charts.ShowNodes;
		return runWithLogLiveStats ? showNodes : true;
	}

	private bool ShowNPS()
	{
		var showNPS = layoutOptions.Charts.ShowNPS;
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
			StopClock();
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

	private static readonly TimeSpan CalmPeriod = TimeSpan.FromMilliseconds(500);
	private static readonly TimeSpan HurryPeriod = TimeSpan.FromMilliseconds(100);

	/// <summary>
	/// The clock ticker, started once per game (GameStarted) and stopped by StopClock() when the
	/// game ends or the page leaves. Each tick works out the remaining time and move time of the
	/// side to move from the clock captured at the last move plus the stopwatch, and hands the two
	/// strings to the clock cells - only when a string actually changed. At the 500ms period with
	/// whole seconds on the face, every second tick used to produce the same text and render it
	/// anyway. Only the side to move is ever notified: the other clock is frozen.
	/// </summary>
	private async void StartTimer()
	{
		if (clockTimer != null) return;
		RestartMoveClock();
		var t = new PeriodicTimer(CalmPeriod);
		clockTimer = t;
		var period = CalmPeriod;
		string lastLeft = null, lastMove = null;
		var lastSide = whiteToMove;

		while (await t.WaitForNextTickAsync())
		{
			try
			{
				MaybeToggleSpeedChart();

				var side = whiteToMove;
				var elapsed = Stopwatch.GetElapsedTime(System.Threading.Volatile.Read(ref moveStartedAt));
				var remaining = (side ? whiteClock : blackClock) - elapsed;
				if (remaining < TimeSpan.Zero) remaining = TimeSpan.Zero;

				// Tenths on the face and ten ticks a second come from the same rule, and both are
				// released again when the side to move is not in trouble - a new game, or the other
				// side with an hour left.
				var hurry = InTimeTrouble(remaining);
				var wanted = hurry ? HurryPeriod : CalmPeriod;
				if (wanted != period) { t.Period = wanted; period = wanted; }

				var left = ClockText(remaining, tenths: hurry);
				var move = ClockText(elapsed, tenths: hurry);
				if (side) { whiteTime = left; whiteMoveTime = move; }
				else { blackTime = left; blackMoveTime = move; }

				if (side != lastSide || left != lastLeft || move != lastMove)
				{
					lastSide = side; lastLeft = left; lastMove = move;
					await Notifier.OnNextTick(side, left, move);
				}
			}
			catch (Exception e)
			{
				logger.LogError(e.Message);
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
				// Always, boards on or off: the panel keeps the position current either way and
				// only RENDERS it when the boards are shown, so switching them on mid-opening shows
				// the position the replay is at, not the start position.
				if (engineStats != null)
					await engineStats.SetPVMoveWithAnnotation(move, true);
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
		StopClock();

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
		// Optional now: a config that says nothing is not wrong, only one that says something odd.
		var configured = layoutOptions?.Sizes?.PVboardSize;
		if (!string.IsNullOrEmpty(configured) && !validSizes.Contains(configured))
			logger.LogInformation("Invalid PV board size in tournament.json: " + configured
				+ " (available: " + string.Join(", ", validSizes) + ")");
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
			RefreshLayoutOptions();   // the screen is known now, so its chart and panel choices apply
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
					// loadTournament sets lastLoadError and hands back Tournament.Empty on ANY failure,
					// a missing file included - so "no error and no engines" means exactly one thing:
					// the file parsed and names no engines yet. That is every fresh installation, and
					// it used to get the same red alert as a broken file, with a text about checking
					// paths that gave that reader nothing to check.
					var loadErr = ChessLibrary.Tournament.Manager.lastLoadError;
					if (tournament == null || !string.IsNullOrEmpty(loadErr))
					{
						tournamentLoadError = !string.IsNullOrEmpty(loadErr) ? loadErr : "tournament.json could not be loaded.";
						StateHasChanged();
						return;
					}
					if (tournament.EngineSetup?.Engines == null || tournament.EngineSetup.Engines.Length == 0)
					{
						firstRun = true;
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
			RefreshLayoutOptions();   // the block and its parts are optional, and this screen may override some
			SetPVStyle();
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
		RefreshLayoutOptions();   // the block and its parts are optional, and this screen may override some
		ValidatePVSize();
		BestMoveWithPolicy = layoutOptions.BestMoveWithPolicy;
		ApplyPvBoardMode();
		autoCycleTimeInSec = layoutOptions.AutoCycleTimeInSec;
		SetPVStyle();
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
		StopClock();
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
