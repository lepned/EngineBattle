// Tournaments: the sentences the page shows about the tournament itself.
//
// The header banner and the description are built from the tournament's own configuration
// rather than from its progress - the mode and its pairing rule, the opening book and how it is
// walked, the adjudication thresholds, how many pairs are done and how long the whole thing has
// been running. Nothing here changes what the tournament does; it only says what it is.
//
// The three modes each phrase themselves differently, which is why Gauntlet, GetCupSummary and
// GetSwissSummary exist side by side instead of as one string with holes in it.

using System.Text.Json;
using ChessLibrary;
using Microsoft.JSInterop;
using MudBlazor;
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
}
