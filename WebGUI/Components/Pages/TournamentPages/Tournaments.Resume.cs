// Tournaments: picking up a cup, swiss or ladder that was already under way.
//
// These three modes keep their progress in a file beside the tournament - a bracket, a swiss
// state, a ladder state - so starting one is never just "run it": the page has to work out
// whether a usable state exists, ask whether to resume or start over, and get the old file out
// of the way if the answer is start over.
//
// The three are near-identical on purpose rather than by accident: each reads a different file
// with a different shape, and the summary each one produces (complete, invalid, resumable) is
// what decides which question the user is asked. Folding them into one would mean a parameter
// for every difference.

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
			{ "FontSize", FontCeiling(FontKey.Brackets) }
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
			{ "FontSize", FontCeiling(FontKey.Brackets) }
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
			{ "FontSize", FontCeiling(FontKey.Brackets)  }
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
}
