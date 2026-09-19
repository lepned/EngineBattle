// Tournaments: everything the running tournament tells this page, and what the page does
// about it.
//
// The runner publishes a stream of Update values - a game started, a move was played, an
// engine reported its search, a pairing list changed, a match was decided - and Update() is the
// switch that turns each of them into page state. The handlers it calls sit beside it, because
// they only exist to serve it.
//
// Two things to keep in mind when changing anything here:
//   - This arrives on the runner's thread, NOT the render dispatcher. Anything that touches
//     component state or renders has to go through InvokeAsync.
//   - What arrives may be the runner's own object rather than a copy. The pairing list is:
//     the swiss and ladder runners mutate one list in place and hand out the same instance
//     each time, so it is copied on receipt before this page reads it.

using ChessLibrary;
using Microsoft.JSInterop;
using MudBlazor;
using WebGUI.Plotting;
using WebGUI.Services;
using static ChessLibrary.EngineTypes;
using static ChessLibrary.LayoutTypes;
using static ChessLibrary.MiscTypes;
using static ChessLibrary.PGNTypes;
using static ChessLibrary.TypesDef;
using static ChessLibrary.TypesDef.CoreTypes;

namespace WebGUI.Components.Pages.TournamentPages;

public partial class Tournaments
{
	private async void Update(TournamentTypes.Update update)
	{
		try
		{
		switch (update)
		{
			case TournamentTypes.Update.GameStarted player:
				await InvokeAsync(StartTimer);
				ResetGameState();
				DrawP = tournament.Adjudication.DrawOption.DrawMoveLength * 2;
				var crossTable = table.FirstOrDefault(e => e.Player == whitePlayer);
				if (crossTable != null)
				{
					var h2h = crossTable.StatsAgainst.FirstOrDefault(e => e.Item1 == blackPlayer);
					if (h2h != null)
					{
						var wdl = h2h.Item2;
						infoBannerInfo.HeadToHead = $"{wdl.Wins}-{wdl.Draws}-{wdl.Losses}";
					}
				}
				if (layoutOptions.ShowCrosstableBetweenGames)
				{
					await InvokeAsync(CloseBetweenGamesDialogs);
				}
				await InvokeAsync(StateHasChanged);
				break;

			case TournamentTypes.Update.EndOfGame e:
				if (tournament.VerboseLogging)
					// Explanation, not ToString — the latter is the short PGN code ("XX").
					logger.LogDebug($"Game over: {e.Result.Reason.Explanation} - deviation counter: {tournament.DeviationCounter}");
				tournamentGamesHeader = $"Last result: {e.Result.Reason}";
				reason = e.Result.Reason.ToString();
				var explanation = e.Result.Reason.Explanation;
				var gameTxtResult = e.Result.Result == "1/2-1/2" ? "Draw" : e.Result.Result == "1-0" ? "White wins" : "Black wins";
				gameResult = $"{gameTxtResult} by {explanation}";
				// U+2011, not a plain hyphen: the move list renders each space-separated token
				// in its own span, and a browser will split "1-0" after the hyphen when the
				// line runs out. nowrap is no use here — the trailing space lives inside the
				// span, so it would stop the list wrapping at all.
				var resultToken = e.Result.Result.Replace('-', '‑');
				var mh = $"{moveHistory}  ({resultToken} : {explanation})";
				moveHistory = mh;
				if (e.Result.Reason != ResultReason.Cancel)
				{
					results.Insert(0, e.Result);
					infoBannerInfo.ResultTxt = e.Result.Result;
				}
				await Task.Delay(200);
				await InvokeAsync(() => streamingBoard.UpdateMoveHistory(mh));
				engineStatus.Clear();
				if (timer != null)
					timer.Dispose();
				timer = null;
				if (oneSecondTimer != null)
					oneSecondTimer.Dispose();
				oneSecondTimer = null;
				if (FeedMode)
					FeedStats.ensureFeedEngines(tournament, results);   // fill roster from result names (no local config)
				var summary = PlayerResults(results);
				scoreTable = summary;
				table = Crosstable(results);
				GameAnalysis.OrdoHelper.populatePairData(summary, table);
				var pgnGames = await UpdateSpeed(0);
				if (pgnGames.Count > 0)
					GameAnalysis.PGNCalculator.populatePentanomialError(summary, pgnGames);
				ApplyPairCount(pgnGames, e.Result.Player1, e.Result.Player2);
				await InvokeAsync(StateHasChanged);
				var resLog = GameAnalysis.OrdoHelper.getResultsAndPairsInConsoleFormat(summary, table);
				if (summary.Count == 2 && pgnGames.Count > 0)
					resLog += Statistics.Pentanomial.formatSingleMatchupCompact(pgnGames);
				Console.WriteLine(resLog);
				// Always written — standings after each game are the point of keeping a log.
				// Verbose puts them on the console too.
				logger.Log(tournament.VerboseLogging ? LogLevel.Information : LogLevel.Debug,
					"{Standings}", StripFences(resLog));

				// Don't pop the between-games crosstable/bracket dialog in feed mode — it's intrusive when
				// just watching a fed tournament (and the layout flag here comes from the fed config).
				if (!FeedMode && layoutOptions.ShowCrosstableBetweenGames)
				{
					if (IsCupMode)
					{
						pendingCupDialog = true;
					}
					else if (IsSwissMode)
					{
						pendingSwissDialog = true;
					}
					else if (IsLadderMode)
					{
						pendingLadderDialog = true;
					}
					else
					{
						await InvokeAsync(OpenDialog);
					}
				}
				break;

			case TournamentTypes.Update.PeriodicResults pr when FeedMode:
				// Feed mode: the join catch-up (global gid) carries the full tournament results as a
				// PeriodicResults -- load it so standings reflect the whole fed tournament, not just the
				// focused game's own EndOfGame events (which is why the table was empty on the focus page).
				results = new List<Result>(pr.results);
				scoreTable = PlayerResults(results);
				table = Crosstable(results);
				GameAnalysis.OrdoHelper.populatePairData(scoreTable, table);
				await InvokeAsync(StateHasChanged);
				break;

			case TournamentTypes.Update.BestMove b:
				boardSyncGen++;   // live position — stop any opening animation still running
				moveHistory = b.Info.MoveHistory;
				FinalStatusReceived(b.Status);
				board.LoadFen(b.Info.MoveAndFen.FenAfterMove);
				var found = openingExplorer.Lookup(board.PositionHash());
				if (found != null)					
					currentOpeningInPlay = $"{found.Name}, ECO: {found.ECO}";
				await streamingBoard.OnNotifyMoveAndFen(b.Info.MoveAndFen);
				TB = b.Info.PiecesLeft - tournament.Adjudication.TBAdj.TBMen;
				move50 = b.Info.Move50;
				r3 = b.Info.R3;
				var white = b.Info.Player == whitePlayer;
				if (b.Info.Eval.IsMate)
					evalList.AddEvalData(white, true, b.Info.Eval.Value);
				else
					evalList.AddEvalData(white, false, b.Info.Eval.Value);
				nodeList.AddData(white, b.Info.Nodes, false);
				npsList.AddData(white, b.Info.NPS, true);				
				if (whiteHasEps || blackHasEps)
					epsList.AddData(white, b.Status.EPS, true);
				timeUsageList.AddData(white, b.Info.MoveTime.TotalSeconds, false);
				var pv = b.Info.PV;
				var pvLong = b.Info.LongPV;
				if (b.Info.Player == blackPlayer)
				{
					Engine2.Eval = b.Info.Eval;
					blackPV = pv;
					blackLongPV = pvLong;
					DrawP = b.Info.AdjDrawML;
					blackClock = b.Info.TimeLeft;
					blackTime = oneSecondTimer != null ? TimeLeftFormatted(b.Info.TimeLeft) : MoveTimeFormatted(b.Info.TimeLeft);
					whiteMoveTime = oneSecondTimer != null ? TimeLeftFormatted(TimeSpan.Zero) : MoveTimeFormatted(TimeSpan.Zero);
					BlackMoveAndFen = b.Info.MoveAndFen;
				}
				else
				{
					Engine1.Eval = b.Info.Eval;
					whitePV = pv;
					whiteLongPV = pvLong;
					DrawP = b.Info.AdjDrawML;
					whiteClock = b.Info.TimeLeft;
					whiteTime = oneSecondTimer != null ? TimeLeftFormatted(b.Info.TimeLeft) : MoveTimeFormatted(b.Info.TimeLeft);
					blackMoveTime = oneSecondTimer != null ? TimeLeftFormatted(TimeSpan.Zero) : MoveTimeFormatted(TimeSpan.Zero);
					WhiteMoveAndFen = b.Info.MoveAndFen;
				}

				whiteToMove = b.Info.Player == blackPlayer;
				var playerTc = whiteToMove ? WhiteEngineConfig.TimeControlID : BlackEngineConfig.TimeControlID;
				infoBannerInfo.TCText = tournament.TimeControlTextForPlayer(playerTc);
				infoBannerInfo.DevCounter = tournament.DeviationCounter;
				await CalcPVAgreement(!whiteToMove);
				fen = b.Info.FEN;

				if (runWithLogLiveStats)
				{
					searchInfoPlot?.ClearSearchData();
				}

				await InvokeAsync(StateHasChanged);
				await DoChartUpdates();
				break;

			case TournamentTypes.Update.Info i:
				break;

			case TournamentTypes.Update.Eval e:
				//SetSpeedFactor();
				break;

			case TournamentTypes.Update.Status s:
				await StatusReceived(s.Engine);
				break;

			case TournamentTypes.Update.Time t:
				if (t.Player == blackPlayer)
				{
					blackClock = t.Time;
					blackTime = TimeLeftFormatted(blackClock);
				}
				else
				{
					whiteClock = t.Time;
					whiteTime = TimeLeftFormatted(whiteClock);
				}
				break;

			case TournamentTypes.Update.NNSeq n:
				try
				{
					if (runWithLogLiveStats && n.NNSeq.Count > 0 && searchInfoPlot != null)
					{
						n.NNSeq.Reverse();
						searchInfoPlot.AddData(n.NNSeq);
						var maxLines = Math.Min(n.NNSeq.Count, layoutOptions.Charts.NumberOfLines);
						await searchInfoPlot.UpdateLogLiveCharts(n.NNSeq, maxLines, layoutOptions.Charts.Qdiff);
					}
				}
				catch (Exception)
				{
					var moves = n.NNSeq.Count;
					var maxLines = Math.Min(moves, layoutOptions.Charts.NumberOfLines);
					var nnItem = n.NNSeq.FirstOrDefault();
					var name = maxLines == 0 ? "Player" : nnItem.Player;
					logger.LogError($"{name} failed to update LogLiveCharts. Available moves: {moves}, required for chart: {maxLines} moves.");
				}

				break;

			case TournamentTypes.Update.StartOfGame start:
				try
				{
					gameResult = string.Empty;
					activeGameNr = start.Game.CurrentGameNr;

					TimeSpan duration = TimeSpan.Zero;
					var roundNumber = 1;
					if (!string.IsNullOrEmpty(roundNr) && roundNr.Contains('.'))
					{
						if (!int.TryParse(roundNr.Split('.')[0], out roundNumber))
							roundNumber = 1;
					}

					var gamesLeft = runner != null ? runner.GetAllPairings().Count : Math.Max(0, totalNumberOfPairs - activeGameNr);
					var gamesLeftForEstimate = IsLadderMode
						? Math.Max(0, tournament.TotalGames - tournament.CurrentGameNr + 1)
						: gamesLeft;

					if (results.Count > 5)
					{
						double avgTimePerGame = results.Average(e => e.GameTime);
						double totalDelay = gamesLeftForEstimate * (tournament.DelayBetweenGames.TotalMilliseconds + 15000);
						var timeLeft = (avgTimePerGame * gamesLeftForEstimate) + totalDelay;
						var timespan = TimeSpan.FromMilliseconds(timeLeft);
						duration = TimeSpan.FromSeconds(Math.Round(timespan.TotalSeconds));
					}
					else if (runner == null || IsLadderMode)
					{
						// feed mode (no runner) or ladder: estimate from completed-game average
						double avgTimePerGame = results.Count == 0 ? 10000 : results.Average(e => e.GameTime);
						duration = TimeSpan.FromMilliseconds(avgTimePerGame * gamesLeftForEstimate);
					}
					else
					{
						var allPairings = runner.GetAllPairings();
						if (allPairings.Count == 0)
						{
							double avgTimePerGame = results.Count == 0 ? 10000 : results.Average(e => e.GameTime);
							duration = TimeSpan.FromMilliseconds(avgTimePerGame * gamesLeftForEstimate);
						}
						else
						{
							var (tTime, _) = ChessLibrary.TournamentRunners.TournamentUtils.estimateTournamentAndGameTime(gamesLeft, tournament, allPairings);
							duration = tTime;
						}
					}

					infoBannerInfo = new InfoBannerInfo(tournament);
					infoBannerInfo.TournamentEndsTxt = InfoBannerInfo.GetTournamentEnd(duration);
					infoBannerInfo.TimeLeftTxt = InfoBannerInfo.Duration(duration);
					ApplyGameProgress();
					infoBannerInfo.Round = roundNr;
					var playerTcId = start.Game.WhiteToMove ? start.Game.WhitePlayer.TimeControlID : start.Game.BlackPlayer.TimeControlID;
					infoBannerInfo.TCText = tournament.TimeControlTextForPlayer(playerTcId);

					if (tournament.PauseAfterRound > 0 && roundNumber > tournament.PauseAfterRound)
					{
						logger.LogInformation($"Tournament paused after round number: {roundNumber - 1}");
						CancelTournament();
					}
					else
					{		
						if (start.Game.OpeningMovesAndFen.Count == 0)
						{
							await streamingBoard.OnNewFen(start.Game.StartPos);
							WhiteMoveAndFen = MoveAndFen.Init(start.Game.StartPos);
							BlackMoveAndFen = MoveAndFen.Init(start.Game.StartPos);
							fen = start.Game.StartPos;
						}
						await GameStartedReceived(start.Game);
					}
					await InvokeAsync(StateHasChanged);
				}
				catch (Exception e)
				{
					logger.LogError(e.Message);
				}				

				break;

			case TournamentTypes.Update.StartOfTournament tournyStart:
				try
				{
					startTournyInfo = tournyStart.Info;
					var games = tournyStart.Info.NumberOfGames;
					var incomingTournament = tournyStart.Info.Tournament.Value;
					if (FeedMode && incomingTournament.EngineSetup != null
						&& (incomingTournament.EngineSetup.Engines == null || incomingTournament.EngineSetup.Engines.Length == 0)
						&& tournament?.EngineSetup?.Engines != null && tournament.EngineSetup.Engines.Length > 0)
					{
						// EngineSetup.Engines (and other [JsonIgnore] fields) don't survive the wire — they
						// are rebuilt from EngineDefList at load. Preserve the locally-loaded values.
						incomingTournament.EngineSetup.Engines = tournament.EngineSetup.Engines;
						incomingTournament.IsChess960 = tournament.IsChess960;
					}
					tournament = incomingTournament;
					// Feed mode has no local roster: Engines is [JsonIgnore], so it arrives null over the
					// wire. Guarantee a non-null list (populated from result player names as they accrue) so
					// the UI's LINQ over Engines never NREs when there's no local tournament.json.
					if (FeedMode)
						FeedStats.ensureFeedEngines(tournament, results);
					UpdateSwissRoundLabel();
					if (pairingTableHeight > 0)
					{
						ApplyPvBoardMode();
						autoCycleTimeInSec = layoutOptions.AutoCycleTimeInSec;
					}
					if (!FeedMode)
					{
						results = runner.GetResults();
						var players = runner.GetPlayerResults(results);
						scoreTable = players;
						table = runner.GenerateStatsCrosstable(results);
						GameAnalysis.OrdoHelper.populatePairData(players, table);
						System.Collections.Generic.List<PGNTypes.PgnGame> gameData = null;
						if (results.Count > 0)
						{
							gameData = await UpdateSpeed(0);
						}
						gameData ??= runner.GetPGNGames();
						GameAnalysis.PGNCalculator.populatePentanomialError(players, gameData);
						ApplyPairCount(gameData);
						var consoleInfo = GameAnalysis.OrdoHelper.getResultsAndPairsInConsoleFormat(players, table);
						if (players.Count == 2)
							consoleInfo += Statistics.Pentanomial.formatSingleMatchupCompact(gameData);
						Console.WriteLine(consoleInfo);
						// Zeroed on a fresh start, carried over on a resume.
						logger.Log(tournament.VerboseLogging ? LogLevel.Information : LogLevel.Debug,
							"{Standings}", StripFences(consoleInfo));
						pairings = runner.GetLastestPairings();
					}
					else
					{
						// Feed mode: standings come from accumulated results; pairings arrive via PairingList events.
						scoreTable = PlayerResults(results);
						table = Crosstable(results);
						GameAnalysis.OrdoHelper.populatePairData(scoreTable, table);
					}
					tournament.TotalGames = totalNumberOfPairs; //runner.TotalGames;
					await InvokeAsync(StateHasChanged);
				}
				catch (Exception e)
				{
					logger.LogError(e.Message);
				}
				break;

			case TournamentTypes.Update.EndOfTournament tourny:
				try
				{
					if (FeedMode)
					{
						// Feed mode: finalize standings from the accumulated results (no local PGN/runner).
						tournament.TotalGames = totalNumberOfPairs;
						scoreTable = PlayerResults(results);
						table = Crosstable(results);
						GameAnalysis.OrdoHelper.populatePairData(scoreTable, table);
					}
					else if (File.Exists(tourny.Info.PgnOutPath))
					{
						var pgnGamesEnd = runner.GetPGNGames();
						var anyGameData = pgnGamesEnd.Any();
						tournament.DeviationCounter = anyGameData ? pgnGamesEnd.Last().GameMetaData.Deviations : 0;
						tournament.TotalGames = totalNumberOfPairs;
						results = runner.GetResults();
						scoreTable = runner.GetPlayerResults(results);
						table = runner.GenerateStatsCrosstable(results);
						GameAnalysis.OrdoHelper.populatePairData(scoreTable, table);
						GameAnalysis.PGNCalculator.populateSpeedMetrics(scoreTable, pgnGamesEnd);
						GameAnalysis.PGNCalculator.populatePentanomialError(scoreTable, pgnGamesEnd);
						ApplyPairCount(pgnGamesEnd);
						if (pairings.Count == 0)
						{
							infoBannerInfo = new InfoBannerInfo(tournament);
							infoBannerInfo.TournamentEndsTxt = InfoBannerInfo.GetTournamentEnd(TimeSpan.Zero);
							infoBannerInfo.TimeLeftTxt = InfoBannerInfo.Duration(TimeSpan.Zero);
						}
						var testTime = TimeSpan.FromMilliseconds(results.Sum(r => r.GameTime));
						var durMsg = $"Total test duration: {testTime.Days} days, {testTime.Hours} hours and {testTime.Minutes} minutes";
						Console.WriteLine(durMsg);
						tournament.PrintTournamentSummary();
						var consoleRes = GameAnalysis.OrdoHelper.getResultsAndPairsInConsoleFormat(scoreTable, table);
						if (scoreTable.Count == 2)
							consoleRes += Statistics.Pentanomial.formatSingleMatchupCompact(pgnGamesEnd);
						Console.WriteLine(consoleRes);
						// The one table worth having in every log, verbose or not.
						logger.LogInformation("{Standings}", StripFences(consoleRes));
					}
					logger.LogInformation("End of tournament: {Name} with deviation counter: {Deviations}",
						tourny.Info.Name, tourny.Info.DeviationCounter);

					await InvokeAsync(StateHasChanged);
				}
				catch (Exception e)
				{
					logger.LogError(e.Message);
				}
				finally
				{
					if (!layoutOptions.ShowCrosstableBetweenGames)
					{
						if (IsCupMode)
							await InvokeAsync(OpenCupBracketDialog);
						else if (IsSwissMode)
						{
							await InvokeAsync(() => OpenSwissDialog(false));
						}
						else if (IsLadderMode)
							await InvokeAsync(OpenLadderDialog);
						else
							await InvokeAsync(OpenDialog);
					}
					if (IsSwissMode)
					{
						var path = Path.Combine(Environment.ContentRootPath, "wwwroot", "swiss_state.json");
						if (File.Exists(path))						
							ChessLibrary.Configuration.ConsoleHelper.writeSwissPairingsPerRoundFromFile(path);
					}
				}
				break;

			case TournamentTypes.Update.MessagesFromEngine sender:
				MessagesFromCeres.Insert(0, sender.Message);
				if (MessagesFromCeres.Count > 10)
				{
					var last = MessagesFromCeres.Last();
					MessagesFromCeres.Remove(last);
				}
				break;
			case TournamentTypes.Update.PairingList list:
				if (runner != null)
					runner.Pairings = list.Pairings;
				var pairingsBefore = pairings.Count;

				// A COPY. The swiss and ladder runners build this list once and then mutate it in
				// place as matches are decided, handing out the same instance every time - so
				// without copying, this component's list is the runner's list: the count compared
				// below was already the new one (the guard could never fire), and a background
				// thread was calling RemoveAll on a list being enumerated here during a render.
				pairings = new List<Pairing>(list.Pairings);
				// Both arrive before the first game starts; render them, or the page shows
				// nothing until StartOfGame - which waits for the engines to come up.
				await InvokeAsync(StateHasChanged);

				// The box this table lives in is MEASURED, and until now it was measured around
				// a single "No pairings available" row: the page is reached with the list empty
				// and it only fills when a run starts. Nothing else re-measures, which is why
				// the box looked wrong until F11 was pressed - that fired a resize by accident.
				if (pairings.Count != pairingsBefore)
					_ = RefitAfterLayoutChange();
				break;
			case TournamentTypes.Update.TotalNumberOfPairs games:
				totalNumberOfPairs = games.PairingsNumber;
				await InvokeAsync(StateHasChanged);
				break;
			case TournamentTypes.Update.RoundNr data:
				roundNr = data.Round;
				break;
			case var cupUpdate when cupUpdate.IsCupBracketUpdated:
				if (IsCupMode)
				{
					await Notifier.NotifyCupUpdated();
					if (pendingCupDialog && layoutOptions.ShowCrosstableBetweenGames)
					{
						pendingCupDialog = false;
						await InvokeAsync(OpenCupBracketDialog);
					}
				}
				break;
			case var swissUpdate when swissUpdate.IsSwissStateUpdated:
				if (IsSwissMode)
				{
					UpdateSwissRoundLabel();
				}
				if (IsSwissMode && layoutOptions.ShowCrosstableBetweenGames)
				{
					if (pendingSwissDialog)
					{
						pendingSwissDialog = false;
						await InvokeAsync(() => OpenSwissDialog(false));
					}
				}
				break;
			case var ladderUpdate when ladderUpdate.IsLadderStateUpdated:
				if (IsLadderMode)
				{
					await Notifier.NotifyLadderUpdated();
					if (pendingLadderDialog && layoutOptions.ShowCrosstableBetweenGames)
					{
						pendingLadderDialog = false;
						await InvokeAsync(OpenLadderDialog);
					}
				}
				break;
			case TournamentTypes.Update.PonderStatus ponderStatus:
				await PonderStatusReceived(ponderStatus.Engine);
				break;
		}
		}
		catch (Exception ex) when (ex is TaskCanceledException or ObjectDisposedException)
		{
			// Circuit disposed (e.g. user navigated away) — safe to ignore
		}
		catch (Exception ex)
		{
			// Update runs as async void on the tournament worker thread: any exception
			// escaping here is unhandled on a thread-pool thread and kills the process.
			logger.LogError(ex, "Error handling tournament update");
		}
	}

	private async Task<System.Collections.Generic.List<PGNTypes.PgnGame>> UpdateSpeed(int delayInMs)
	{
		// The entry is timestamped; the wait is what's worth recording.
		if (delayInMs > 0)
			await Task.Delay(delayInMs);
		if (tournament.VerboseLogging)
			logger.LogDebug("Speed update after {Delay}ms delay", delayInMs);
		if (runner == null)
			return new System.Collections.Generic.List<PGNTypes.PgnGame>();   // feed mode: no local PGN games
		var games = runner.GetPGNGames();
		GameAnalysis.PGNCalculator.populateSpeedMetrics(scoreTable, games);
		return games;
	}

	private async Task OpenQuickDialog()
	{
		if (IsCupMode)
		{
			await OpenCupBracketDialog();
		}
		else if (IsSwissMode)
		{
			await OpenSwissDialog(true);
		}
		else
		{
			await OpenDialog();
		}
	}

	private async Task GameStartedReceived(StartGameInfo info)
	{
		boardSyncGen++;   // new game supersedes any older opening animation still in flight
		SetGameParams(info);
		board.ResetBoardState();
		currentOpeningInPlay = info.OpeningName;
		// Try hash-based opening detection for better names
		if (info.OpeningMovesAndFen.Count > 0)
		{
			try
			{				
				OpeningInfo best = null;
				foreach (var entry in info.OpeningMovesAndFen)
				{
					board.LoadFen(entry.FenAfterMove);
					var found = openingExplorer.Lookup(board);
					if (found != null)
						best = found;
				}
				if (best != null)
					currentOpeningInPlay = $"Opening: {best.Name}, ECO: {best.ECO}";
			}
			catch { }
		}
		moveNr = 0;
		TB = 0;
		UpdateDeviationReference(info);
		whitePlayer = info.WhitePlayer.Name;
		whiteDev = info.WhitePlayer.Dev;
		if (String.IsNullOrEmpty(info.WhitePlayer.LogoPath))
			whiteEngineLogo = "Img/chessLogo.jpg";
		else
			whiteEngineLogo = info.WhitePlayer.LogoPath;
		whiteClock = info.WhiteTime;
		whiteTime = TimeLeftFormatted(whiteClock);
		blackPlayer = info.BlackPlayer.Name;
		blackDev = info.BlackPlayer.Dev;
		if (String.IsNullOrEmpty(info.BlackPlayer.LogoPath))
			blackEngineLogo = "Img/chessLogo.jpg";
		else
			blackEngineLogo = info.BlackPlayer.LogoPath;
		blackClock = info.BlackTime;
		blackTime = TimeLeftFormatted(blackClock);
		whiteToMove = info.WhiteToMove;
		WhiteEngineConfig = info.WhitePlayer;
		BlackEngineConfig = info.BlackPlayer;
		Engine1 = EngineStatus.Empty;
		Engine1.PlayerName = whitePlayer;
		Engine2 = EngineStatus.Empty;
		Engine2.PlayerName = blackPlayer;
		blackMoveTime = TimeLeftFormatted(TimeSpan.Zero);
		whiteMoveTime = TimeLeftFormatted(TimeSpan.Zero);
		if (runner != null)
			pairings = runner.GetLastestPairings();   // feed mode: pairings arrive via PairingList events
		ResetGameState();
		await PlayOpeningMoves(new List<MoveAndFen>(info.OpeningMovesAndFen));
	}

	private void UpdateDeviationReference(StartGameInfo info)
	{

		openingSansForDeviation = info.OpeningMovesAndFen?
			.Select(m => m.ShortSan)
			.Where(s => !string.IsNullOrWhiteSpace(s))
			.ToList() ?? new List<string>();

		referenceSansForDeviation = new List<string>();

		if (runner is null)
			return;

		try
		{
			var currentWhite = info.WhitePlayer.Name?.Trim() ?? string.Empty;
			var currentBlack = info.BlackPlayer.Name?.Trim() ?? string.Empty;
			var openingName = info.OpeningName?.Trim() ?? string.Empty;
			var startPos = info.StartPos?.Trim() ?? string.Empty;
			var allPairings = runner.GetAllPairings;

			var games = runner.GetPGNGames() ?? new List<ChessLibrary.PGNTypes.PgnGame>();
			var previousPairGame = games
				.Where(g =>
				{					
					var openingHash = g.GameMetaData.OpeningHash?.Trim() ?? string.Empty;
					var w = g.GameMetaData.White?.Trim() ?? string.Empty;
					var b = g.GameMetaData.Black?.Trim() ?? string.Empty;
					var isSwapped =
						string.Equals(w, currentBlack, StringComparison.OrdinalIgnoreCase) &&
						string.Equals(b, currentWhite, StringComparison.OrdinalIgnoreCase);
					var sameStart = info.OpeningHash == openingHash;					
					return isSwapped && sameStart;
				})
				.OrderByDescending(g => g.GameNumber)
				.FirstOrDefault();

			if (previousPairGame is null)
				return;

			var sans = new List<string>();
			foreach (var move in previousPairGame.Mainline)
			{				
				sans.Add(move.San.Trim());				
			}

			referenceSansForDeviation = sans;
		}
		catch (Exception ex)
		{
			logger.LogDebug(ex, "Failed to build deviation reference for move list.");
		}
	}

	// TimeSpan has no "HH" specifier, and its "hh" wraps at a day — which a long time
	// control now legitimately passes. Hours are carried whole, and a clock that has run
	// past zero is clamped here: the overshoot is real internally, where adjudication
	// needs it, but on screen a negative clock reads as a bug.
	private static string ClockText(TimeSpan t, bool tenths = false)
	{
		if (t < TimeSpan.Zero) t = TimeSpan.Zero;
		return tenths
			? $"{(int)t.TotalHours:00}:{t.Minutes:00}:{t.Seconds:00}.{t.Milliseconds / 100}"
			: $"{(int)t.TotalHours:00}:{t.Minutes:00}:{t.Seconds:00}";
	}

	private string MoveTimeFormatted(TimeSpan time) => ClockText(time, tenths: true);
	private string OneSecondMoveTimeFormatted(TimeSpan time) => ClockText(time);
	private string TimeLeftFormatted(TimeSpan time) => ClockText(time);

	private void FinalStatusReceived(EngineStatus info)
	{
		SetEngineStatus(info);
		
		if (info.WDL.IsHasValue)
		{
			switch (info.WDL)
			{
				case WDLType.HasValue wdl:
					if (info.PlayerName == whitePlayer)
						whiteWDL = GetWDLString(wdl.Values);

					else
						blackWDL = GetWDLString(wdl.Values);
					break;
			}
		}
	}

	private async Task PonderStatusReceived(EnginePonderStatus info)
	{
		SetEnginePonderStatus(info);
		await InvokeAsync(StateHasChanged);
	}

	private async Task StatusReceived(EngineStatus info)
	{
		try
		{
			SetEngineStatus(info);
			var pv = info.PV;

			if (!info.Eval.IsNA)
			{
				if (whiteToMove)
				{
					whitePV = pv;
					whiteLongPV = info.PVLongSAN;
				}

				else
				{
					blackPV = pv;
					blackLongPV = info.PVLongSAN;
				}

				var bArr = blackLongPV.Split(" ");
				var wArr = whiteLongPV.Split(" ");
				if (bArr.Length == 0 || wArr.Length == 0)
				{
					logger.LogWarning("Empty PV arrays in StatusReceived.");
					return;
				}
				var nAgreement = await CalcPVAgreement(whiteToMove);

				if (info.PlayerName == Engine2.PlayerName)
				{
					var withPolicy = blackLogLive && BestMoveWithPolicy;
					if (bArr.Length > 0)
					{
						if (nAgreement == 0 && wArr.Length > 1)
						{
							await streamingBoard.UpdateLastPVMove(bArr[0], wArr[1], !withPolicy);
						}
						else
						{
							await streamingBoard.UpdateLastPVMove(bArr[0], string.Empty, !withPolicy);
						}
					}
					Engine2.Eval = info.Eval;
				}
				else
				{

					var withPolicy = whiteLogLive && BestMoveWithPolicy;
					if (wArr.Length > 0)
					{
						if (nAgreement == 0 && bArr.Length > 1)
						{
							await streamingBoard.UpdateLastPVMove(wArr[0], bArr[1], !withPolicy);
						}
						else
						{
							await streamingBoard.UpdateLastPVMove(wArr[0], string.Empty, !withPolicy);
						}

					}
					Engine1.Eval = info.Eval;
				}
			}

			if (info.WDL.IsHasValue)
			{
				switch (info.WDL)
				{
					case WDLType.HasValue wdl:
						//var sharpness = CalcSharpness(wdl.Values);
						//logger.LogInformation(wdl.Values.ToString());
						//logger.LogInformation("Sharpness value: " + sharpness.ToString("F1"));
						if (info.PlayerName == whitePlayer)
							whiteWDL = GetWDLString(wdl.Values);
						else
							blackWDL = GetWDLString(wdl.Values);
						break;
				}
			}

			await InvokeAsync(StateHasChanged);
		}
		finally
		{
			//_semaphore.Release();
		}

	}

	private async Task DoChartUpdates()
	{
		try
		{
			await Task.WhenAll(
				evalList.SetEvalChartData(),
				nodeList.SetChartNodeData(),
				npsList.SetChartNodeData(),
				epsList.SetChartNodeData(),
				timeUsageList.SetChartTimeUsageData());
		}

		catch (JSException ex)
		{
			var msg = $"An error occurred in JS {nameof(DoChartUpdates)}: {ex.Message}";
			logger.LogError(msg);
			Console.WriteLine(msg);
		}

		catch (Exception ex)
		{
			// Handle the exception here
			var msg = $"An error occurred in {nameof(DoChartUpdates)}: {ex.Message}";
			logger.LogError(msg);
			Console.WriteLine(msg);
			Console.WriteLine(ex.InnerException);
		}
	}
}
