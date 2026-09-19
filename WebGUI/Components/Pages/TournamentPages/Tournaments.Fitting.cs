// Tournaments: measuring the layout, and sizing the regions that have to fit inside it.
//
// The tournament page cannot predict how much room anything gets. The window, the drawer, the
// board size, the number of charts and the engine names all move it, so the page measures what
// the browser actually laid out and answers with a size. Everything in this file is that: one
// debounced, generation-guarded pass that runs on every resize, four regions whose WIDTH is
// measured, and three boxes whose HEIGHT is measured from the space left below them.
//
// Two rules worth keeping:
//   - Widths run last and nothing runs after them. Re-running a height pass afterwards hands a
//     box more room, the row clamp raises the size again, and the table overflows with nothing
//     left to measure it.
//   - An overflow ratio is never below 1: a box can report too little room, never room to
//     spare. Growing back therefore needs its own trigger, which is the box's own width
//     changing - see ReleaseWidthFitsWhoseBoxChanged.
//
// The sizes themselves come from tournament.json and are spent in CSS; docs/FontScalingPlan.md
// has the design. The corner control that nudges them lives in Tournaments.SizeControls.cs.

using Microsoft.JSInterop;
using static ChessLibrary.LayoutTypes;

namespace WebGUI.Components.Pages.TournamentPages;

public partial class Tournaments
{
	/// <summary>Everything measureTournamentLayout() reports, in one payload.</summary>
	private sealed record LayoutMetrics(
		double WindowHeight,
		double UnzoomedViewportHeight,
		double Lhs,
		double Rhs,
		double StandingTable);

	// Resize events arrive in bursts -- one F11 toggle or window drag produces several -- and
	// this method is a long async sequence. Without a generation guard two runs interleave, and
	// the measured fit at the end of one corrects a layout the other is still changing. Only the
	// newest run is allowed to continue.
	private int _resizeGeneration;

	private bool IsSupersededResize(int generation) =>
		generation != System.Threading.Volatile.Read(ref _resizeGeneration);

	[JSInvokable]
	public async Task OnBrowserResize()
	{
		var generation = System.Threading.Interlocked.Increment(ref _resizeGeneration);

		await Task.Delay(300);
		if (IsSupersededResize(generation)) return;

		var nCharts = CalcNumberOfChartsToShow();

		// One round-trip for every measurement. These used to be six separate interop calls,
		// which on Blazor Server are six messages over the circuit -- per resize event, and
		// resize events arrive in bursts.
		var metrics = await chessModule.InvokeAsync<LayoutMetrics>("measureTournamentLayout");
		if (IsSupersededResize(generation)) return;

		windowHeight = metrics.WindowHeight;
		var elementsAboveCycleTableHeight = metrics.Rhs;
		var standingTableHeight = metrics.StandingTable;
		standingsTableHeightPx = standingTableHeight;
		int buffer = 140; //CalcBufferFunction((int)metrics.UnzoomedViewportHeight);
		var elementsAboveStandings = metrics.Lhs + buffer;
		if (showPVBoard)
		{
			elementsAboveStandings += 35;
		}

		if (!layoutOptions.OnlyShowStandings)
		{
			elementsAboveStandings += 10;
		}

		var absTableDiff = Math.Abs(windowHeight - elementsAboveCycleTableHeight);

		// The move list is no longer predicted from the board, PV and banner heights plus a
		// constant. FitMoveListToMeasuredSlack() below measures the space the layout actually
		// left, which also drops three interop round-trips per resize.

		if (windowHeight > elementsAboveStandings)
		{
			height = Math.Max(50, (int)(windowHeight - elementsAboveStandings)) - 5;
			shouldCycle = standingTableHeight > height;
		}

		if (absTableDiff > 0)
		{
			pairingTableHeight = Math.Max(0, (int)(absTableDiff - (65 + 27 * nCharts)));
		}

		await InvokeAsync(StateHasChanged);
		await Task.Delay(200);

		if (IsSupersededResize(generation)) return;
		await FitMoveListToMeasuredSlack(generation);

		if (IsSupersededResize(generation)) return;
		await FitStandingsToMeasuredSlack(generation);

		if (IsSupersededResize(generation)) return;
		await FitPairingsToMeasuredSlack(generation);

		// Widths last, and nothing after them. Running the height passes again afterwards is
		// tempting - a width fit changes a text size, which changes a height - but it undoes the
		// width: the height pass hands the standings box more room, the row clamp raises the
		// size again, and the table goes back to overflowing with nothing left to measure it.
		//
		// The cost of stopping here: a width fit that RELEASES a region (see
		// ReleaseWidthFitsWhoseBoxChanged) grows its text, and the heights measured above were
		// measured smaller. That only happens when a box actually changed width, which only
		// happens during a resize - and resize events arrive in bursts, so the last one in the
		// burst finds the box already recorded, releases nothing, and leaves the heights valid.
		if (IsSupersededResize(generation)) return;
		await FitWidths(generation);
	}

	// Small gap kept under the last PV box so it never sits flush against the bottom edge.
	private const int PvBottomPadding = 8;

	/// <summary>
	/// Sizes the move list from the space the layout actually left over, rather than from part
	/// heights plus tuned constants.
	///
	/// The constants above encode one particular viewport height — a maximised browser, with its
	/// chrome. Any host with a different viewport height (browser F11, a WebView shell, a
	/// different monitor) pushed the white PV box past the bottom edge, where .main-screen's
	/// overflow:hidden left it unreachable.
	///
	/// The move list is the only elastic region in that column, so one pass converges: a pixel of
	/// move-list height moves the PV box by exactly one pixel.
	/// </summary>
	/// <summary>
	/// Grows (or shrinks) the standings box to the space the layout actually left below it.
	///
	/// The height above is predicted: window height minus the measured elements above minus a
	/// tuned buffer. The buffer encodes one particular set of chrome - it was calibrated when
	/// the app bar existed - so on any other layout the box ends up short of the bottom edge
	/// and clips rows that had room, or overshoots and pushes itself past it. Standings.razor
	/// caps the table with max-height, so a box that is too short simply hides engines.
	///
	/// The box is the elastic region in its column, so one pixel of height moves its bottom
	/// edge by one pixel and a pass converges. The cycling decision follows the new height:
	/// once the table fits, there is nothing to cycle through.
	/// </summary>
	private async Task FitStandingsToMeasuredSlack(int generation)
	{
		if (chessModule is null) return;

		for (var pass = 0; pass < 2; pass++)
		{
			if (IsSupersededResize(generation)) return;

			// 0 also means "no standings on this layout" (cup, ladder, feed view), which
			// correctly leaves the height alone.
			var slack = await chessModule.InvokeAsync<double>("getSlackBelow", "#standingsDiv");
			if (Math.Abs(slack) <= 2) return;

			var fitted = Math.Clamp(height + (int)slack - StandingsBottomPadding, 50, (int)windowHeight);
			if (fitted == height) return;

			height = fitted;
			// A table that now fits must stop cycling, and one that still does not must keep it.
			shouldCycle = standingsTableHeightPx > height;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}
	}

	// ── Width fits ───────────────────────────────────────────────────────────────

	/// <summary>
	/// How much of its configured size one region may use once its content has been measured
	/// against the box it has to fit in. 1 means it fits.
	///
	/// Four regions want exactly this and differ only in what to measure and how far they may
	/// shrink, so they share one type and one loop.
	///
	/// LastBox is what makes growing back possible. An overflow ratio is never below 1 - a box
	/// cannot report that it has room to spare, only that it has too little - so a fit that
	/// only ever divided would ratchet downwards and stay there when the window was widened or
	/// engines left. When the box's own width changes, the fit starts again from the full size
	/// and re-derives; when it does not, nothing has happened that could free up room.
	/// </summary>
	private sealed class RegionWidthFit(string selector, double floor)
	{
		public string Selector { get; } = selector;
		public double Floor { get; } = floor;
		public double Value { get; set; } = 1.0;
		public double LastBox { get; set; }
		public void Reset() { Value = 1.0; LastBox = 0; }
	}

	/// The crosstable grows in both directions with the field; 0.55 is as small as it may get.
	private readonly RegionWidthFit crosstableFit = new("#crosstableDiv", 0.55);
	/// Both strips of the header banner; the wider one decides.
	private readonly RegionWidthFit bannerFit = new(".infoBanner, .infoBannerContent", 0.5);
	/// The setup lines. A high floor, because past it the CSS ellipsis is the better answer.
	private readonly RegionWidthFit descriptionFit = new("li.eb-g-description", 0.8);
	/// The standings table, whose six headings are what run out of room first. A high floor:
	/// past it the table is not worth reading, and the name column's own character limit gives
	/// up far more width far more cheaply before this ever binds.
	private readonly RegionWidthFit standingsFit = new("#standingsDiv", 0.8);

	private RegionWidthFit[] WidthFits => [crosstableFit, bannerFit, descriptionFit, standingsFit];

	/// <summary>
	/// Lowers each region's ceiling until its content stops running off the side of its box.
	///
	/// The regions measured here have a width that no formula predicts: the crosstable's
	/// columns and headers grow together, the banner carries whatever hardware line the user
	/// typed, and the setup lines are built from the tournament's own settings. What they have
	/// in common is that their content is proportional to their text size, so one division
	/// lands on the answer rather than searching for it.
	/// </summary>
	private async Task FitWidths(int generation)
	{
		if (chessModule is null) return;

		// Releasing first, then deriving, in two separate steps. Doing both in one loop meant a
		// region could take the release branch on the LAST pass: it would be rendered back at
		// full size and never measured again, which left it overflowing until something else
		// triggered a resize. Now every pass of the loop below is a measurement.
		if (await ReleaseWidthFitsWhoseBoxChanged(generation))
		{
			if (IsSupersededResize(generation)) return;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}

		for (var pass = 0; pass < 2; pass++)
		{
			if (IsSupersededResize(generation)) return;

			var changed = false;
			foreach (var fit in WidthFits)
			{
				var m = await chessModule.InvokeAsync<OverflowMeasure>("measureOverflow", fit.Selector);
				if (m is null || m.Box <= 0) continue;      // not on screen in this layout
				if (!(m.Ratio >= 1)) continue;              // also catches NaN, if the contract changes

				var fitted = Math.Clamp(fit.Value / m.Ratio, fit.Floor, 1.0);
				if (Math.Abs(fitted - fit.Value) < 0.02) continue;

				fit.Value = fitted;
				changed = true;
			}

			if (!changed) return;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}
	}

	/// <summary>
	/// Puts back to full size every region whose box has changed width since it was last fitted,
	/// so the loop that follows re-derives from scratch. True when anything moved.
	///
	/// This is the only thing standing between the fit and a one-way ratchet, because an
	/// overflow ratio is never below 1: a box can say it has too little room, never that it has
	/// room to spare.
	/// </summary>
	private async Task<bool> ReleaseWidthFitsWhoseBoxChanged(int generation)
	{
		var released = false;
		foreach (var fit in WidthFits)
		{
			if (IsSupersededResize(generation)) return released;

			var m = await chessModule.InvokeAsync<OverflowMeasure>("measureOverflow", fit.Selector);
			if (m is null || m.Box <= 0 || m.Box == fit.LastBox) continue;

			fit.LastBox = m.Box;
			if (fit.Value == 1.0) continue;
			fit.Value = 1.0;
			released = true;
		}
		return released;
	}

	/// <summary>
	/// Gives every width fit back its full size, for when the CONTENT changes rather than the
	/// box: a new tournament brings a different hardware line, different engine names and a
	/// different number of columns, and a fit measured against the old one has no claim on the
	/// new. The box-width check cannot see this, because the box did not move.
	/// </summary>
	private void ReleaseWidthFitsForNewContent()
	{
		foreach (var fit in WidthFits) fit.Reset();
	}

	/// <summary>What measureOverflow() reports: how far past its box the content runs, and how
	/// wide that box is.</summary>
	private sealed class OverflowMeasure
	{
		public double Ratio { get; set; } = 1.0;
		public double Box { get; set; }
	}

	/// <summary>
	/// Grows the upcoming-pairings box to the space the layout actually left below it.
	///
	/// Its height was predicted the way the standings box used to be: the window height minus
	/// the measured elements above, minus a tuned constant that grows with the number of charts.
	/// A constant like that encodes one particular set of chrome, so on any other layout the box
	/// stops short of the bottom edge and shows fewer pairings than there was room for - which
	/// is exactly what it did. The box is the elastic region at the foot of its column, so one
	/// pixel of height moves its bottom edge by one pixel and a pass converges.
	/// </summary>
	private async Task FitPairingsToMeasuredSlack(int generation)
	{
		if (chessModule is null) return;

		for (var pass = 0; pass < 2; pass++)
		{
			if (IsSupersededResize(generation)) return;

			// 0 also means "not on this layout", which correctly leaves the height alone.
			var slack = await chessModule.InvokeAsync<double>("getSlackBelow", "#pairingsBox");
			if (Math.Abs(slack) <= 2) return;

			var fitted = Math.Clamp(pairingTableHeight + (int)slack - PairingsBottomPadding, 50, (int)windowHeight);
			if (fitted == pairingTableHeight) return;

			pairingTableHeight = fitted;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}
	}

	private const int PairingsBottomPadding = 8;

	private const int StandingsBottomPadding = 8;
	/// The measured height of the standings table itself, kept from the last resize so the
	/// cycling decision can be revisited when the box is refitted.
	private double standingsTableHeightPx;

	private async Task FitMoveListToMeasuredSlack(int generation)
	{
		if (chessModule is null) return;

		// Two passes, because growing the move list can reflow a neighbour and break the
		// one-pixel-for-one-pixel assumption. In the common case the second pass is a no-op.
		for (var pass = 0; pass < 2; pass++)
		{
			if (IsSupersededResize(generation)) return;

			// 0 also means "no PV box on this layout", which correctly leaves the height alone.
			var slack = await chessModule.InvokeAsync<double>("getSlackBelow", ".pv-white-box");

			// Ignore sub-pixel churn so a resize storm cannot oscillate the layout.
			if (Math.Abs(slack) <= 2) return;

			var fitted = Math.Clamp(moveListHeight + (int)slack - PvBottomPadding, 0, (int)windowHeight);
			if (fitted == moveListHeight) return;

			moveListHeight = fitted;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}
	}
}
