// Tournaments: the controls that decide how big things are, and where the numbers live.
//
// The sizes on this page belong to the SCREEN, not to the tournament. The number that makes a
// 24-engine standings readable on a 4K monitor is wrong on the laptop panel it docks to, and
// it has nothing to do with which engines are playing - so every size here is read from the
// settings, under a key for the screen the page is on, and written there by the controls on
// the page itself. Nobody has to open a JSON file to make the text bigger.
//
// tournament.json USED to carry a font size per region and two chart heights, and "save" wrote
// the sizes on screen back into it. Those fields are optional now and only consulted for a
// screen that has nothing saved yet - an older config keeps rendering as it did, right up to
// the first time a slider is moved or "save" is pressed. Then the settings own it.
//
// Three layers, each with a job:
//   - a CEILING per region, in px (FontCeiling): this screen's saved number, else the file's,
//     else the built-in default;
//   - a NUDGE per screen (fontScalePct, A-/A+), multiplied into every ceiling, for "a bit
//     bigger" without opening anything;
//   - the MEASURING in Tournaments.Fitting.cs, which may lower a region further so it fits.
// "save" collapses the first two: the sizes on screen become the ceilings and the nudge goes
// back to 100%, so the numbers in Appearance are the numbers on the page.
//
// The design, and why the ceiling is a ceiling rather than an answer, is in
// docs/FontScalingPlan.md.

using Microsoft.JSInterop;
using MudBlazor;
using WebGUI.Services;
using static ChessLibrary.LayoutTypes;

namespace WebGUI.Components.Pages.TournamentPages;

public partial class Tournaments
{
	// ── Where the numbers come from ──────────────────────────────────────────────

	/// This screen's saved sizes, or null when it has none yet.
	private GlobalSettings.TournamentScreenLayout ScreenLayout =>
		SettingsService.Settings.TournamentLayoutFor(fontScaleScreenKey);

	/// <summary>
	/// The largest size a region's text may reach, in px, before the nudge and the clamp: this
	/// screen's saved number, else what tournament.json says (older configs), else the built-in
	/// default. Never zero - a zero ceiling is the floor, and a config that leaves a field out
	/// deserialises to zero.
	/// </summary>
	private int FontCeiling(string group)
	{
		if (ScreenLayout?.FontPx is { } saved && saved.TryGetValue(group, out var px) && px > 0)
			return px;
		var file = FileFontSize(group);
		return file > 0 ? file : GlobalSettings.DefaultFontPx(group);
	}

	/// What tournament.json asks for, per region, or 0 when it says nothing.
	private int FileFontSize(string group) => GlobalSettings.FileFontPx(layoutOptions, group);

	// ── Which charts and panels ──────────────────────────────────────────────────
	// The screen decides these too. Rather than teach every read site about the settings, the
	// choices are folded INTO layoutOptions when it is (re)built: a copy of the normalised block
	// with this screen's non-null choices substituted. Every existing read of
	// layoutOptions.Charts.X or layoutOptions.OnlyShowStandings then sees the effective value.

	/// The block as this screen shows it.
	private LayoutOption ApplyScreenDisplay(LayoutOption lo)
	{
		var l = ScreenLayout;
		if (l is null) return lo;
		var c = lo.Charts;
		var charts = new Charts(
			l.ShowNps ?? c.ShowNPS,
			l.ShowEval ?? c.ShowEval,
			l.ShowNodes ?? c.ShowNodes,
			l.ShowTime ?? c.ShowTime,
			l.NumberOfLines is > 0 ? l.NumberOfLines.Value : c.NumberOfLines,
			l.Qdiff is > 0 ? l.Qdiff.Value : c.Qdiff);
		return new LayoutOption(
			lo.Fonts, lo.Sizes, charts, lo.ShowPVBoard,
			l.UseNpm ?? lo.UseNPM,
			lo.BestMoveWithPolicy,   // file only, by decision: not ready for a GUI control
			lo.OnlyShowStandings,                  // legacy flags, read for old files only
			l.ShowCrosstableBetweenGames ?? lo.ShowCrosstableBetweenGames,
			lo.ShowCrosstableBelowStandings,
			ParseCrosstable(l.CrosstableWithStandings) ?? lo.CrosstableWithStandings,
			l.AutoCycleTimeInSec is > 0 ? l.AutoCycleTimeInSec.Value : lo.AutoCycleTimeInSec);
	}

	/// The settings spell the choice as it is spelled in tournament.json; null when not set.
	private static CrosstableWithStandings ParseCrosstable(string s) => s switch
	{
		"cycle" => CrosstableWithStandings.Cycle,
		"below" => CrosstableWithStandings.Below,
		"none" => CrosstableWithStandings.Hidden,
		_ => null
	};

	/// Rebuilds layoutOptions from the tournament and this screen. Called where the tournament is
	/// loaded, when the screen key arrives, and after any choice made on this page.
	private void RefreshLayoutOptions()
	{
		layoutOptions = ApplyScreenDisplay(LayoutOption.Normalize(tournament?.LayoutOption));
		BestMoveWithPolicy = layoutOptions.BestMoveWithPolicy;
	}

	/// The four chart toggles in the corner. A chart appearing or going changes the height of
	/// everything below it, so the same re-measure as a chart-height change follows.
	private void ToggleChart(string which)
	{
		var settings = SettingsService.Settings;
		var l = settings.TournamentLayoutForWrite(fontScaleScreenKey);
		var c = layoutOptions.Charts;
		switch (which)
		{
			case "eval": l.ShowEval = !c.ShowEval; break;
			case "nps": l.ShowNps = !c.ShowNPS; break;
			case "time": l.ShowTime = !c.ShowTime; break;
			case "nodes": l.ShowNodes = !c.ShowNodes; break;
			default: return;
		}
		SettingsService.Save(settings);
		RefreshLayoutOptions();
		_ = InvokeAsync(OnBoardSizeChanged);
	}

	// ── Chart height ─────────────────────────────────────────────────────────────
	// The same three layers as the text: a height per screen (or the file's, or 200), a nudge
	// on the corner control, and "save" folding the nudge into the height. Seven charts can
	// share that column, so how much room each gets is a screen decision.

	private int ChartHeightPx(bool live)
	{
		var saved = live ? ScreenLayout?.LiveChartHeight ?? 0 : ScreenLayout?.MoveChartHeight ?? 0;
		if (saved > 0) return saved;
		var file = live ? layoutOptions?.Sizes?.LiveChartHeight ?? 0 : layoutOptions?.Sizes?.MoveChartHeight ?? 0;
		return file > 0 ? file : 200;
	}

	private string liveChartStyle => ChartHeightStyle(ChartHeightPx(live: true));
	private string moveChartStyle => ChartHeightStyle(ChartHeightPx(live: false));

	private string ChartHeightStyle(int px) =>
		"height:"
		+ Math.Round(px * chartScalePct / 100.0).ToString(System.Globalization.CultureInfo.InvariantCulture)
		+ "px;";

	/// A chart's height after the nudge, as the whole number the settings store.
	private int ScaledChartHeight(int px) => (int)Math.Round(px * chartScalePct / 100.0);

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

	// ── PV boards ────────────────────────────────────────────────────────────────
	// Whether the two PV boards are shown and how big is a screen decision too: the right
	// answer for a two-engine broadcast is the wrong one as soon as several games share the
	// screen and the row has nowhere to go. "off" removes the row rather than collapsing it.
	private static readonly (string Mode, string Label, string Tip)[] PvBoardChoices =
	{
		("off", "off", "No PV boards, and the row they sat in goes with them"),
		("small", "S", "Small PV boards"),
		("medium", "M", "Medium PV boards"),
		("large", "L", "Large PV boards"),
	};

	/// The mode in force: this screen's choice, else what tournament.json says, else medium.
	private string PvBoardMode
	{
		get
		{
			var saved = ScreenLayout?.PvBoard;
			if (!string.IsNullOrEmpty(saved) && PvBoardChoices.Any(c => c.Mode == saved)) return saved;
			var fallback = LayoutOption.Default.Sizes.PVboardSize;
			if (layoutOptions is null) return fallback;
			if (!layoutOptions.ShowPVBoard) return "off";
			var size = layoutOptions.Sizes?.PVboardSize;
			return size is not null && validSizes.Contains(size) ? size : fallback;
		}
	}

	/// Keeps the render flag in step with the mode. Called where the old code read
	/// layoutOptions.ShowPVBoard directly, so the guards around those reads are untouched.
	private void ApplyPvBoardMode() => showPVBoard = PvBoardMode != "off";

	// ── How big the two PV boards actually are ───────────────────────────────────
	// The share of the row each mode asks for. These used to be percentages in a global
	// stylesheet, which meant the size lived somewhere no control on this page could reach:
	// the S/M/L buttons above, reset and - the one that mattered - the main board's own size
	// slider all changed the column and then had the result clipped by a CSS number none of
	// them could see. The boards were the last in the app sized by a class rather than by a
	// pixel value from here; every other one (LiveFeedGrid, PVtileBoard, StreamingChessboard,
	// ModernChessboard) already took SizePx.
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
	/// something oversized.
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
		if (mode == PvBoardMode) return;

		var settings = SettingsService.Settings;
		settings.TournamentLayoutForWrite(fontScaleScreenKey).PvBoard = mode;
		SettingsService.Save(settings);

		ApplyPvBoardMode();
		_ = RefitAfterLayoutChange();
	}

	// ── Text nudge ───────────────────────────────────────────────────────────────
	// One nudge, multiplied into the ceiling of every region (docs/FontScalingPlan.md). Kept
	// per screen: the number that makes a 24-engine standings readable on a 4K monitor is not
	// the number that suits the laptop panel it docks to, and being asked to redo it on every
	// dock is the fiddling this replaces.
	private int fontScalePct = 100;
	private string fontScaleScreenKey = "";

	private void NudgeFontScale(int deltaPct) => SetFontScale(fontScalePct + deltaPct);

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

	// ── Reset and save ───────────────────────────────────────────────────────────

	/// <summary>
	/// Forgets everything this screen chose - sizes, PV boards, chart heights, both nudges -
	/// so the page shows what tournament.json says, or the built-in defaults. Only THIS
	/// screen's entries go: another screen's choices were made looking at that screen.
	/// </summary>
	private void ResetToConfiguredLayout()
	{
		var settings = SettingsService.Settings;
		settings.TournamentLayoutByScreen.Remove(GlobalSettings.ScreenBucket(fontScaleScreenKey));
		// Directly, not through SetFontScale: that returns early when the control already reads
		// 100%, which would leave a stored nudge surviving a button whose tooltip promises
		// everything goes back.
		settings.TournamentFontScale = 1.0;
		settings.TournamentFontScaleByScreen.Remove(fontScaleScreenKey);
		settings.TournamentChartScale = 1.0;
		settings.TournamentChartScaleByScreen.Remove(fontScaleScreenKey);
		SettingsService.Save(settings);

		fontScalePct = 100;
		chartScalePct = 100;
		RefreshLayoutOptions();   // charts and panels back to the file, or the defaults
		ApplyPvBoardMode();
		SetPVStyle();   // the PV label ceiling is a stored style, not a computed one
		ReleaseWidthFitsForNewContent();   // measured ceilings are not "what the defaults say"
		foreach (var fit in WidthFits) fit.Reset();

		// Always the full re-measure with Plotly told: a chart may have appeared or gone, or
		// changed height, and a plain refit would leave the plots at their old size.
		_ = InvokeAsync(OnBoardSizeChanged);
	}

	/// <summary>
	/// Makes the sizes on screen this screen's own sizes.
	///
	/// The point of the nudge is to find a set of sizes by looking rather than by typing
	/// numbers; this is how that answer becomes the baseline, so the sliders in Appearance show
	/// the sizes on the page and the nudge is free for the next adjustment. What is written is
	/// what is on screen: the ceiling, the nudge and the clamp have all been applied already,
	/// which is why the tables are MEASURED rather than recomputed here. A table the clamp had
	/// to shrink is saved shrunk - that IS what is on screen - and "reset" is the way back.
	///
	/// Only this screen's entry changes, and it can be pressed at any time: it touches nothing
	/// but the settings, so a live run and a feed view - someone sizing someone else's
	/// tournament for their own screen - are exactly the cases it is for.
	/// </summary>
	private async Task SaveAsBaseline()
	{
		try
		{
			var sizes = await EffectiveFontSizes();
			if (sizes.Count == 0)
			{
				Snackbar.Add("Nothing to save: no tournament tables are on screen", Severity.Warning);
				return;
			}

			var settings = SettingsService.Settings;
			var layout = settings.TournamentLayoutForWrite(fontScaleScreenKey);
			var written = 0;
			foreach (var (group, px) in sizes)
			{
				if (px <= 0) continue;
				// Never the brackets: what is on screen under .eb-g-brackets is the cup or ladder
				// progress table, and that is rendered at the STANDINGS ceiling. Saving its measured
				// size here would overwrite the size of the swiss/cup/ladder overviews - which the
				// user sets in Appearance - with the standings size. The old file write-back had
				// this same rule; it belongs to the region, not to where it used to be written.
				if (group == FontKey.Brackets) continue;
				layout.FontPx[group] = px;
				written++;
			}

			// The chart heights are not measured: no clamp touches them, so what is on screen is
			// exactly the height times the nudge, and arithmetic beats a round trip to the browser.
			if (chartScalePct != 100)
			{
				layout.LiveChartHeight = ScaledChartHeight(ChartHeightPx(live: true));
				layout.MoveChartHeight = ScaledChartHeight(ChartHeightPx(live: false));
				written += 2;
			}

			// The sizes now contain what the nudge produced, so keeping the nudge would apply it
			// a second time. Only THIS screen's entry goes; the shared default belongs to this
			// screen only when this screen has no key of its own.
			if (string.IsNullOrEmpty(fontScaleScreenKey))
			{
				settings.TournamentFontScale = 1.0;
				settings.TournamentChartScale = 1.0;
			}
			settings.TournamentFontScaleByScreen.Remove(fontScaleScreenKey);
			settings.TournamentChartScaleByScreen.Remove(fontScaleScreenKey);
			SettingsService.Save(settings);
			fontScalePct = 100;
			chartScalePct = 100;

			SetPVStyle();   // the PV label ceiling is a stored style, not a computed one
			ReleaseWidthFitsForNewContent();
			foreach (var fit in WidthFits) fit.Reset();   // the new ceilings have not been measured yet
			Snackbar.Add($"{written} sizes saved for this screen", Severity.Success);
			await InvokeAsync(StateHasChanged);
			await OnBrowserResize();
		}
		catch (Exception ex)
		{
			logger.LogError(ex, "Failed to save the sizes on screen");
			Snackbar.Add($"Could not save sizes: {ex.Message}", Severity.Error);
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

		var scale = SettingsService.Settings.FontScaleFor(fontScaleScreenKey);
		foreach (var group in GlobalSettings.FontGroups)
		{
			if (measured is not null && measured.TryGetValue(group.Key, out var px) && px > 0)
			{
				sizes[group.Key] = px;
				continue;
			}
			// Not on screen, or not a region the clamp touches: ceiling times the nudge - and
			// times its width fit, for the two that have one. The banner and the description
			// are measured for WIDTH even though nothing clamps their height, so leaving the fit
			// out here would save a bigger number than what is on screen.
			sizes[group.Key] = (int)Math.Round(FontCeiling(group.Key) * scale * WidthFitFor(group.Key));
		}
		return sizes;
	}

	/// The width fit in force for a region, or 1 for the regions that have none.
	private double WidthFitFor(string group) => group switch
	{
		FontKey.Banner => bannerFit.Value,
		FontKey.Description => descriptionFit.Value,
		_ => 1.0
	};
}
