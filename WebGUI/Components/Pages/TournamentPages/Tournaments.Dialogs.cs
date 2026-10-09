// Tournaments: the dialogs this page opens over itself.
//
// All of them are the same shape - close whatever is open, show one, remember its reference so
// it can be closed again - which is the whole reason they sit together. CloseAllDialogs is what
// keeps two from stacking when the page reacts to something while one is already up.

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
		gameResult = "1-0";
		runner?.AdjudicateGame(activeGameNr, "1-0");
		StopClock();
		await Task.CompletedTask;
	}

	async Task BlackWins()
	{
		if (AdjudicationBlocked()) return;
		gameResult = "0-1";
		runner?.AdjudicateGame(activeGameNr, "0-1");
		StopClock();
		await Task.CompletedTask;
	}
	async Task DrawGame()
	{
		if (AdjudicationBlocked()) return;
		gameResult = "1/2-1/2";
		runner?.AdjudicateGame(activeGameNr, "1/2-1/2");
		StopClock();
		await Task.CompletedTask;
	}

	async Task OpenDialog()
	{
		if (results.Count > 3000)
			return;

		CloseAllDialogs();

		var players = tournament?.EngineSetup?.Engines?.Count() ?? 0;
		var fontSize = FontCeiling(FontKey.Crosstable) + 1;
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
			{ "FontSize", FontCeiling(FontKey.Brackets) }
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
			{ "BracketPath", GetCupBracketPath() },
			{ "FontSize", FontCeiling(FontKey.Brackets) }
		};
		bracketDialogReference = await DialogService.ShowAsync<Components.Layout.TournamentLayout.CupBracketDialog>("", parameters, options);
	}

	async Task OpenLadderDialog()
	{
		CloseAllDialogs();
		await Task.Delay(1000);
		// as wide as the resume prompt at startup, so the table looks the same in both (as the
		// Swiss overview and the cup bracket do)
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
			{ "FontSize", FontCeiling(FontKey.Brackets) }
		};
		ladderDialogReference = await DialogService.ShowAsync<Components.Layout.TournamentLayout.LadderResumeDialog>("", parameters, options);
	}

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
}
