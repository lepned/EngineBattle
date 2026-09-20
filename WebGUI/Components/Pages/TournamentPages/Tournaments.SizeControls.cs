// Tournaments: the controls that decide how big things are, and what they write to.
//
// tournament.json carries a size for every region of this page. That answer is right for the
// screen and the field it was written for and wrong for the next one, so it is treated as a
// CEILING rather than an instruction: the control in the bottom-right corner nudges it, and
// the measuring in Tournaments.Fitting.cs may lower it further to make something fit.
//
// Everything here is that control and its consequences - the text nudge, the chart height, the
// PV boards, putting it all back, and baking the result into tournament.json so the next run
// starts from it. The nudges are kept per SCREEN, because the number that suits a 4K monitor
// is not the one that suits the laptop panel it docks to.
//
// The design, and why the file is a ceiling rather than an answer, is in
// docs/FontScalingPlan.md.

using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.JSInterop;
using MudBlazor;
using WebGUI.Services;
using static ChessLibrary.LayoutTypes;

namespace WebGUI.Components.Pages.TournamentPages;

public partial class Tournaments
{
	/// The two chart heights from tournament.json, after the user's nudge. Computed rather than
	/// assigned: they were being rebuilt in two places, and the nudge is a third input.
	private string liveChartStyle => ChartHeightStyle(layoutOptions.Sizes.LiveChartHeight);
	private string moveChartStyle => ChartHeightStyle(layoutOptions.Sizes.MoveChartHeight);

	private string ChartHeightStyle(int configured) =>
		"height:"
		+ Math.Round((configured > 0 ? configured : 200) * chartScalePct / 100.0)
			.ToString(System.Globalization.CultureInfo.InvariantCulture)
		+ "px;";

	// ── PV boards ────────────────────────────────────────────────────────────────
	// tournament.json says whether the two PV boards are shown and how big. That is the right
	// answer for a two-engine broadcast and the wrong one as soon as several games share the
	// screen, where the row simply has nowhere to go - so this overrides the file for this
	// installation, and "off" removes the row rather than collapsing it.
	private static readonly (string Mode, string Label, string Tip)[] PvBoardChoices =
	{
		("off", "off", "No PV boards, and the row they sat in goes with them"),
		("small", "S", "Small PV boards"),
		("medium", "M", "Medium PV boards"),
		("large", "L", "Large PV boards"),
	};

	/// The user's override, or "" while the file's answer stands.
	private string pvBoardChoice = "";

	/// What tournament.json asks for, as one of the four modes.
	private string ConfiguredPvBoardMode =>
		!layoutOptions.ShowPVBoard ? "off"
		: validSizes.Contains(layoutOptions.Sizes.PVboardSize) ? layoutOptions.Sizes.PVboardSize
		: "medium";

	/// The mode actually in force.
	private string PvBoardMode =>
		PvBoardChoices.Any(c => c.Mode == pvBoardChoice) ? pvBoardChoice : ConfiguredPvBoardMode;

	/// Keeps the render flag in step with the mode. Called where the old code read
	/// layoutOptions.ShowPVBoard directly, so the guards around those reads are untouched.
	private void ApplyPvBoardMode() => showPVBoard = PvBoardMode != "off";

	// ── How big the two PV boards actually are ───────────────────────────────────
	// The share of the row each mode asks for. These used to be percentages in a global
	// stylesheet, which meant the size lived somewhere no control on this page could reach:
	// tournament.json, the S/M/L buttons above, ResetToConfiguredLayout and - the one that
	// mattered - the main board's own size slider all changed the column and then had the
	// result clipped by a CSS number none of them could see. The boards were the last in the app
	// sized by a class rather than by a pixel value from here; every other one (LiveFeedGrid,
	// PVtileBoard, StreamingChessboard, ModernChessboard) already took SizePx.
	//
	// The share is of the ENGINE PANEL's width, which is the row the two boards are cells of -
	// they are grid cells of the panel now, not a component below it, so each one is centred on
	// its own engine's column.
	private static readonly Dictionary<string, double> PvBoardShare = new()
	{
		["small"] = 0.23,
		["medium"] = 0.29,
		["large"] = 0.35,
	};

	/// <summary>
	/// Where the row stops counting. These boards are square, so their width is their height,
	/// and a share of a column that grows with the window has no ceiling at all - measured at a
	/// fixed 1100px window height, a board went 39px at a 1400px window to 144px at 2560px, and
	/// that came straight out of the tables below.
	///
	/// ONE number rather than one cap per mode. Three caps are three chances to drift apart, and
	/// they were saying the same thing three times over: 150/0.23, 190/0.29 and 230/0.35 are all
	/// about 655. Expressed this way the modes keep their proportions above the ceiling as well
	/// as below it, which three flat caps would have flattened.
	///
	/// 660 is just above what the row reaches on a 4K panel at 150% scaling (2560 CSS px), so no
	/// board on a screen these are watched on gets smaller - the ceiling only stops the climb
	/// past that. An earlier attempt calibrated on a 1920px window instead, and that visibly
	/// shrank the boards; do not lower this without measuring the screen it will be seen on.
	/// </summary>
	private const double PvBoardRowCeilingPx = 660;

	/// <summary>
	/// The size for one PV board, or null while the panel has not been measured yet - before the
	/// first layout pass. Null leaves the board filling its grid cell, which is its engine's
	/// column: a sensible size rather than nothing, and slightly smaller than any of the three
	/// modes, so the first paint settles UP to the measured size instead of jumping down from
	/// something oversized. The percentages this used to fall back to are gone with the old
	/// stylesheet row.
	/// </summary>
	private int? PvBoardSizePx
	{
		get
		{
			if (pvRowWidthPx <= 0 || !PvBoardShare.TryGetValue(PvBoardMode, out var share))
				return null;
			return (int)Math.Round(Math.Min(pvRowWidthPx, PvBoardRowCeilingPx) * share);
		}
	}

	private void SetPvBoard(string mode)
	{
		if (mode == PvBoardMode && pvBoardChoice == mode) return;
		pvBoardChoice = mode;

		var settings = SettingsService.Settings;
		settings.TournamentPvBoard = mode;
		SettingsService.Save(settings);

		ApplyPvBoardMode();
		_ = RefitAfterLayoutChange();
	}

	/// Gives both the text size and the PV boards back to tournament.json.
	private void ResetToConfiguredLayout()
	{
		pvBoardChoice = "";
		var settings = SettingsService.Settings;
		settings.TournamentPvBoard = "";
		// Directly, not through SetFontScale: that returns early when the control already reads
		// 100%, which would leave a per-region nudge set in Appearance surviving a button whose
		// tooltip says everything goes back to the file.
		settings.TournamentFontScale = 1.0;
		settings.TournamentFontScaleByScreen.Remove(fontScaleScreenKey);
		settings.TournamentFontScaleByGroup.Clear();
		settings.TournamentChartScale = 1.0;
		settings.TournamentChartScaleByScreen.Remove(fontScaleScreenKey);
		SettingsService.Save(settings);

		var chartsMoved = chartScalePct != 100;
		fontScalePct = 100;
		chartScalePct = 100;
		ApplyPvBoardMode();
		ReleaseWidthFitsForNewContent();   // measured ceilings are not "what the file says"

		// Chart containers that changed height need Plotly told, exactly as SetChartScale does;
		// a plain refit only re-measures and would leave the plots at their old size.
		if (chartsMoved) _ = InvokeAsync(OnBoardSizeChanged);
		else _ = RefitAfterLayoutChange();
	}

	// ── Table text scale ─────────────────────────────────────────────────────────
	// One nudge, multiplied into the ceiling every table takes from tournament.json
	// (docs/FontScalingPlan.md). It is kept per screen: the number that makes a 24-engine
	// standings readable on a 4K monitor is not the number that suits the laptop panel it
	// docks to, and being asked to redo it on every dock is the fiddling this replaces.
	private int fontScalePct = 100;
	private string fontScaleScreenKey = "";

	private void NudgeFontScale(int deltaPct) => SetFontScale(fontScalePct + deltaPct);

	// ── Chart height ─────────────────────────────────────────────────────────────
	// The same arrangement as the text nudge, one level up: tournament.json says how tall a
	// chart should be and this says how much of that to use. Seven charts can be on screen at
	// once, so this is the difference between seeing three of them and seeing all of them.
	private int chartScalePct = 100;

	private void NudgeChartScale(int deltaPct) => SetChartScale(chartScalePct + deltaPct);

	private void SetChartScale(int pct)
	{
		pct = Math.Clamp(pct, 50, 200);
		if (pct == chartScalePct) return;
		chartScalePct = pct;

		var settings = SettingsService.Settings;
		var scale = Math.Round(pct / 100.0, 2);
		if (string.IsNullOrEmpty(fontScaleScreenKey))
			settings.TournamentChartScale = scale;
		else
			settings.TournamentChartScaleByScreen[fontScaleScreenKey] = scale;   // see SetFontScale
		SettingsService.Save(settings);

		// A chart does not notice that its container changed height; Plotly has to be told, and
		// the whole column below it has to be re-measured. OnBoardSizeChanged already does both.
		_ = InvokeAsync(OnBoardSizeChanged);
	}

	private void SetFontScale(int pct)
	{
		pct = Math.Clamp(pct, 60, 160);
		if (pct == fontScalePct) return;
		fontScalePct = pct;

		var settings = SettingsService.Settings;
		var scale = Math.Round(pct / 100.0, 2);
		if (string.IsNullOrEmpty(fontScaleScreenKey))
			settings.TournamentFontScale = scale;
		else
			// Stored even at 1.0. Removing the entry looks tidier but hands the screen back to
			// the shared default, which another screen may have set to something else - the
			// control would then read 100% while the page rendered something else.
			settings.TournamentFontScaleByScreen[fontScaleScreenKey] = scale;
		SettingsService.Save(settings);

		// The layout listens for the save and re-emits --eb-font-scale on <html>; the tables
		// inherit it from there, so nothing here touches them. Their BOXES are measured,
		// though, so the fit has to run once more after the new size has been laid out.
		_ = RefitAfterLayoutChange();
	}

	private async Task RefitAfterLayoutChange()
	{
		// Long enough for the re-render to put the new sizes in the document; OnBrowserResize
		// is generation-guarded, so an extra call is never a problem.
		// Every caller starts this and walks away, so an exception here would go unobserved -
		// and one of those callers is the tournament's update stream, mid-run.
		try
		{
			await Task.Delay(120);
			await InvokeAsync(StateHasChanged);
			// InvokeAsync on the resize too: this is reached from that update stream as well as
			// from button clicks, and the stream does not run on the dispatcher.
			await InvokeAsync(OnBrowserResize);
		}
		catch (Exception ex)
		{
			logger.LogDebug("Refit after a layout change failed: {Message}", ex.Message);
		}
	}

	/// <summary>
	/// Makes the sizes on screen the file's own sizes.
	///
	/// The point of the nudge is to find a set of sizes by looking rather than by editing JSON;
	/// this is how that answer gets back into the file, so it survives a reinstall, travels to
	/// another machine, and keeps working for anyone who prefers the text editor. What is
	/// written is what is on screen: the ceiling, the nudge and the clamp have all been applied
	/// already, which is why the tables are MEASURED rather than recomputed here.
	///
	/// Only the twelve font numbers change. The file is parsed as a document rather than as a
	/// Tournament record, so everything else - ordering, fields this build does not know about,
	/// the user's own layout of the file - comes back out exactly as it went in. A copy is kept
	/// beside it regardless.
	/// </summary>
	private async Task SaveSizesToTournamentJson()
	{
		// The markup hides the button in these cases, but a click can land as a run starts, and
		// the reload afterwards reads a cached tournament while one is running.
		if (FeedMode || TournamentSvc.IsRunning)
		{
			Snackbar.Add("Sizes can only be saved between runs", Severity.Warning);
			return;
		}

		var path = System.IO.Path.Combine(System.Environment.CurrentDirectory, "wwwroot", "tournament.json");
		if (!System.IO.File.Exists(path))
		{
			Snackbar.Add($"No tournament.json at {path}", Severity.Warning);
			return;
		}

		var confirmed = await DialogService.ShowMessageBox(
			"Save these sizes to tournament.json?",
			$"The twelve font sizes in the file are replaced by the sizes on screen right now. "
			+ "Nothing else in the file changes, and a copy is kept as tournament.json.bak.",
			yesText: "Save", cancelText: "Cancel");
		if (confirmed != true) return;

		try
		{
			var sizes = await EffectiveFontSizes();
			if (sizes.Count == 0)
			{
				Snackbar.Add("Nothing to save: no tournament tables are on screen", Severity.Warning);
				return;
			}

			var root = System.Text.Json.Nodes.JsonNode.Parse(await System.IO.File.ReadAllTextAsync(path));
			if (root?["LayoutOption"]?["Fonts"] is not System.Text.Json.Nodes.JsonObject fonts)
			{
				Snackbar.Add("tournament.json has no LayoutOption.Fonts to write to", Severity.Error);
				return;
			}

			var written = 0;
			var baked = new List<string>();
			foreach (var group in GlobalSettings.FontGroups)
			{
				if (group.JsonFields.Length == 0) continue;   // nudgeable, but nothing to write it to
				if (!sizes.TryGetValue(group.Key, out var px) || px <= 0) continue;
				foreach (var field in group.JsonFields) fonts[field] = px;
				baked.Add(group.Key);
				written++;
			}

			// The chart heights live in a different part of the file and are not measured: no
			// clamp touches them, so what is on screen is exactly the configured number times
			// the nudge, and arithmetic beats a round trip to the browser.
			var chartsBaked = chartScalePct == 100;   // nothing to bake is the same as baked
			if (!chartsBaked)
			{
				if (root?["LayoutOption"]?["Sizes"] is not System.Text.Json.Nodes.JsonObject chartSizes)
				{
					// Say so rather than clearing a setting that was never preserved anywhere.
					Snackbar.Add("tournament.json has no LayoutOption.Sizes: chart heights not saved", Severity.Error);
					return;
				}
				chartSizes["LiveChartHeight"] = ScaledChartHeight(layoutOptions.Sizes.LiveChartHeight);
				chartSizes["MoveChartHeight"] = ScaledChartHeight(layoutOptions.Sizes.MoveChartHeight);
				written += 2;
			}

			System.IO.File.Copy(path, path + ".bak", overwrite: true);
			await System.IO.File.WriteAllTextAsync(path,
				root.ToJsonString(new System.Text.Json.JsonSerializerOptions { WriteIndented = true }));

			// The file now contains what the nudge produced, so keeping the nudge would apply it
			// a second time. Only what was actually baked is cleared: a region that was not on
			// screen kept its nudge because nothing was written for it, and clearing it would
			// throw away a setting without having preserved it anywhere.
			//
			// Only THIS screen's entry goes. Another screen's nudge was never applied to these
			// numbers, and on that screen it is still the right answer relative to them.
			var settings = SettingsService.Settings;
			// The shared default is what every screen WITHOUT an entry of its own uses, so it
			// only belongs to this screen when this screen has no key. Clearing it otherwise
			// would silently re-render every other screen, which is the opposite of what the
			// comment above promises.
			if (string.IsNullOrEmpty(fontScaleScreenKey))
			{
				settings.TournamentFontScale = 1.0;
				settings.TournamentChartScale = 1.0;
			}
			settings.TournamentFontScaleByScreen.Remove(fontScaleScreenKey);
			settings.TournamentChartScaleByScreen.Remove(fontScaleScreenKey);
			foreach (var key in baked) settings.TournamentFontScaleByGroup.Remove(key);
			SettingsService.Save(settings);
			fontScalePct = 100;
			chartScalePct = 100;

			ReloadLayoutFromConfig();
			Snackbar.Add($"Saved {written} sizes to tournament.json", Severity.Success);
			await OnBrowserResize();
		}
		catch (Exception ex)
		{
			logger.LogError(ex, "Failed to write font sizes to tournament.json");
			Snackbar.Add($"Could not write tournament.json: {ex.Message}", Severity.Error);
		}
	}

	/// <summary>
	/// The size each region actually ended up with. Tables are measured, because the clamp may
	/// have overruled their ceiling; the regions nothing clamps are worked out here, because
	/// for them the size on screen IS the ceiling times the nudge and asking the browser would
	/// only add a round trip and a way to get an empty answer.
	/// </summary>
	private async Task<Dictionary<string, int>> EffectiveFontSizes()
	{
		var sizes = new Dictionary<string, int>();
		if (chessModule is null) return sizes;

		var selectors = GlobalSettings.FontGroups
			.Where(g => g.Selector is not null)
			.ToDictionary(g => g.Key, g => g.Selector);
		var measured = await chessModule.InvokeAsync<Dictionary<string, int>>("getComputedFontSizes", selectors);

		foreach (var group in GlobalSettings.FontGroups)
		{
			if (measured is not null && measured.TryGetValue(group.Key, out var px) && px > 0)
			{
				sizes[group.Key] = px;
				continue;
			}
			// Not on screen, or not a region the clamp touches: ceiling times this region's
			// nudge - and times its width fit, for the two that have one. The banner and the
			// description are measured for WIDTH even though nothing clamps their height, so
			// leaving the fit out here would write a bigger number than what is on screen.
			var configured = ConfiguredFontSize(group.Key);
			if (configured <= 0) continue;
			var scale = SettingsService.Settings.FontScaleFor(fontScaleScreenKey, group.Key);
			sizes[group.Key] = (int)Math.Round(configured * scale * WidthFitFor(group.Key));
		}
		return sizes;
	}

	/// A chart's configured height after the nudge, as the whole number the file stores.
	private int ScaledChartHeight(int configured) =>
		(int)Math.Round((configured > 0 ? configured : 200) * chartScalePct / 100.0);

	/// The width fit in force for a region, or 1 for the regions that have none.
	private double WidthFitFor(string group) => group switch
	{
		"banner" => bannerFit.Value,
		"description" => descriptionFit.Value,
		_ => 1.0
	};

	/// The size tournament.json currently asks for, per region.
	private int ConfiguredFontSize(string group) => group switch
	{
		"standings" => layoutOptions.Fonts.StandingsFont,
		"crosstable" => layoutOptions.Fonts.CrossTableFont,
		"pairings" => layoutOptions.Fonts.PairingsFont,
		"latest" => layoutOptions.Fonts.LatestGamesFont,
		"brackets" => layoutOptions.Fonts.CupBracketFont,
		"movelist" => layoutOptions.Fonts.MoveListFont,
		"enginepanel" => layoutOptions.Fonts.EnginesPanelFont,
		"banner" => layoutOptions.Fonts.InfoBannerFont,
		"description" => layoutOptions.Fonts.TournamentDescFont,
		"pv" => layoutOptions.Fonts.PVLabelFont,
		_ => 0
	};

	/// Re-reads tournament.json and re-applies the parts of the page that are built from its
	/// layout block. Safe only between runs, which is the only time the save button is offered.
	private void ReloadLayoutFromConfig()
	{
		var fresh = TournamentSvc.GetConfigRunner(logger).Tournament();
		if (fresh is null) return;
		tournament = fresh;
		ReleaseWidthFitsForNewContent();
		layoutOptions = tournament.LayoutOption;
		SetPVStyle(layoutOptions);
		foreach (var fit in WidthFits) fit.Reset();   // the new ceilings have not been measured yet
		StateHasChanged();
	}
}
