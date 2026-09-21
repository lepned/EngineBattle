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
		double StandingTable,
		double PvRow);

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
		pvRowWidthPx = metrics.PvRow;
		var elementsAboveCycleTableHeight = metrics.Rhs;
		var standingTableHeight = metrics.StandingTable;
		standingsTableHeightPx = standingTableHeight;
		int buffer = 140; //CalcBufferFunction((int)metrics.UnzoomedViewportHeight);
		var elementsAboveStandings = metrics.Lhs + buffer;
		// No PV allowance here any more. The 35 stood for the 1.5rem margins above and below the
		// PV row's own wrapper div, which offsetHeight does not count and sum('lhs') therefore
		// missed. That div is gone - the boards are a row of the engine panel, inside .lhs, and
		// their only extra height is padding, which offsetHeight DOES count. Keeping the 35
		// handed the standings box 35px less than the layout actually had.

		if (!layoutOptions.CrosstableWithStandings.IsHidden)
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

	/// <summary>
	/// The air kept under the LAST box in each of the three columns - the PV box, the pairings
	/// box and the standings box - so none of them sits flush against the bottom edge.
	///
	/// One number, not one per column: this is a single visual property of the page, and three
	/// constants that must stay equal are three places to forget the third. Raising it is the
	/// only way to get more room at the foot of the page; every fit here is defined as "fill
	/// down to the window minus this", so space freed anywhere above is taken by the box that
	/// grows into it.
	/// </summary>
	private const int BottomPadding = 10;

	/// <summary>
	/// The engine panel's width, as last measured. Zero until the first pass, and the two PV
	/// boards simply fill their grid cell until then.
	///
	/// This is a WIDTH feeding a width, not content feeding a size: it is read only by
	/// PvBoardSizePx in Tournaments.SizeControls.cs, which sizes the two boards, and nothing
	/// downstream of it changes this number back. The spiral the font work ran into - a measured
	/// content height deciding a font that decides that height - needs the loop to close, and
	/// here it does not.
	/// </summary>
	private double pvRowWidthPx;

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
	/// <summary>
	/// The standings box, whichever table is in it at the moment. Every mode cycles something
	/// else through that one place: cup and ladder their progress tables, and round-robin and
	/// swiss the crosstable. The cycling components render one or the other, never both, so a
	/// single element matches - and the layout where the crosstable is a second box below
	/// standings, the one case where two would match, is routed to the two-box method instead.
	///
	/// Used for HEIGHT. The crosstable is deliberately absent from the width list below: it has
	/// its own fit with a lower floor, and two fits writing to one element would fight.
	/// </summary>
	private const string StandingsBoxSelector =
		"#standingsDiv, #cupProgressDiv, #ladderProgressDiv, #crosstableDiv";

	/// The tables that take the standings font ceiling, for WIDTH.
	private const string StandingsTablesSelector = "#standingsDiv, #cupProgressDiv, #ladderProgressDiv";

	/// What one box was given, and what it wants. See measureBoxHeights in chessInterop.js.
	private sealed record BoxHeight(bool Found, double Box, double Content);

	/// <summary>
	/// True when the crosstable is a second box UNDER the standings box rather than something
	/// cycled through the same one. Only then do two boxes share the column.
	/// </summary>
	private bool CrosstableSitsBelowStandings =>
		layoutOptions.CrosstableWithStandings.IsBelow && !IsCupMode && !IsLadderMode;

	/// <summary>
	/// Shares the column between the standings box and the crosstable under it.
	///
	/// Only standings had a cap. The crosstable was rendered without a Height, so it took its
	/// natural height and its font stayed at the configured ceiling, and standings got whatever
	/// was left - which with a full field is almost nothing. Because the font follows the box
	/// height, standings then lost twice: a short box AND small text beside a crosstable at full
	/// size.
	///
	/// So both get a cap, and the column is split by ROW COUNT - the same denominator the CSS
	/// clamp uses, rows plus a slot for the section heading and one for the table's own header
	/// row. Both tables show one line per engine, so that is close to an even split, and both
	/// clamps then land on the same font: they shrink together, in step, which is the point.
	///
	/// Not by measured content height, which was the first attempt and spiralled. Content height
	/// is a function of the font, and the font is what this method is about to decide. The
	/// crosstable is wide, so its width fit lowers its ceiling; the font falls, the content gets
	/// shorter, it is handed a smaller share, and the font falls again - down to the floor, where
	/// it sits too small and stops responding to size at all. Row counts do not move when the
	/// font does, so there is no loop to fall down.
	/// </summary>
	private async Task FitStandingsAndCrosstable(int generation)
	{
		for (var pass = 0; pass < 2; pass++)
		{
			if (IsSupersededResize(generation)) return;

			var boxes = await chessModule.InvokeAsync<BoxHeight[]>(
				"measureBoxHeights", "#standingsDiv", "#crosstableDiv");
			if (IsSupersededResize(generation)) return;

			// Not both on screen yet. Nothing is fitted this pass rather than guessing at a
			// split from one box; the next resize finds them both.
			if (boxes.Length < 2 || !boxes[0].Found || !boxes[1].Found) return;

			var slack = await chessModule.InvokeAsync<double>("getSlackBelow", "#crosstableDiv");
			if (IsSupersededResize(generation)) return;

			var total = boxes[0].Box + boxes[1].Box + slack - BottomPadding;
			if (total < 100) return;

			// +2 apiece, matching the clamp: a slot for the section heading and one for the
			// table's own header row. Equal fields give an even split and an equal font.
			var standingsRows = scoreTable.Count + 2;
			var crosstableRows = table.Count + 2;
			var rows = standingsRows + crosstableRows;

			var standings = (int)Math.Round(total * standingsRows / (double)rows);

			// The floor is applied to the split, not to each cap on its own. Clamping them
			// independently let the pair sum to MORE than the column they were dividing - a
			// lopsided field in a short column gave 93 and 17, the 17 came up to 50, and the
			// lower table hung 33px past the window edge and stayed there, because the next pass
			// recomputes the same pair and finds nothing to change.
			var fittedStandings = Math.Clamp(standings, 50, (int)total - 50);
			var fittedCrosstable = (int)total - fittedStandings;

			if (Math.Abs(fittedStandings - height) <= 2
				&& Math.Abs(fittedCrosstable - crosstableHeight) <= 2) return;

			height = fittedStandings;
			crosstableHeight = fittedCrosstable;
			shouldCycle = standingsTableHeightPx > height;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}
	}

	private async Task FitStandingsToMeasuredSlack(int generation)
	{
		if (chessModule is null) return;

		// Two boxes in the column is a different problem from one: the space under standings is
		// not free, the crosstable is standing in it.
		if (CrosstableSitsBelowStandings)
		{
			await FitStandingsAndCrosstable(generation);
			return;
		}

		for (var pass = 0; pass < 2; pass++)
		{
			if (IsSupersededResize(generation)) return;

			// 0 also means "no standings box on this layout" (the feed view), which correctly
			// leaves the height alone. It must NOT mean "standings is not the table showing right
			// now": cup and ladder put a progress table in the same box under its own id, and the
			// cycling view swaps in the crosstable under a third. Asking only for #standingsDiv
			// found nothing while any of those was up, so the box kept its predicted height and
			// stopped short of the window bottom, growing only in the seconds standings was in.
			var boxes = await chessModule.InvokeAsync<BoxHeight[]>(
				"measureBoxHeights", StandingsBoxSelector);
			if (IsSupersededResize(generation)) return;
			if (boxes.Length == 0 || !boxes[0].Found) return;

			var slack = await chessModule.InvokeAsync<double>("getSlackBelow", StandingsBoxSelector);
			if (IsSupersededResize(generation)) return;
			if (Math.Abs(slack) <= 2) return;

			// The box's MEASURED height plus the room under it, not the cap it was last given.
			// A table with fewer rows than its cap allows is shorter than that cap, so adding the
			// slack to the cap counted the same empty space twice and the loop could never close
			// it: every pass handed out more, up to the whole window. Harmless while the box was
			// standings alone - a cap above the content changes nothing - but cup and ladder
			// share this one height between two tables of different length, so whichever one the
			// cycle timer happened to be showing decided the cap, and the taller one then hung
			// below the window edge. Measured height plus slack is the room from the box's own
			// top edge to the bottom of the window, which does not depend on how full it is.
			var fitted = Math.Clamp(
				(int)(boxes[0].Box + slack) - BottomPadding, 50, (int)windowHeight);
			if (Math.Abs(fitted - height) <= 2) return;

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
	/// The standings box. The fit belongs to the BOX, not to one table: measuring only the
	/// standings meant the cup table kept its full size while the standings beside it shrank, so
	/// cycling between them looked like the font growing every time the column got narrower.
	///
	/// A high floor: past it the table is not worth reading, and the name column's own character
	/// limit gives up far more width far more cheaply before this ever binds.
	private readonly RegionWidthFit standingsFit = new(StandingsTablesSelector, 0.8);

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

			var fitted = Math.Clamp(pairingTableHeight + (int)slack - BottomPadding, 50, (int)windowHeight);
			if (fitted == pairingTableHeight) return;

			pairingTableHeight = fitted;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}
	}



	/// The crosstable's cap in the layout that puts it below standings; 0 elsewhere, which leaves
	/// it at its natural height as before.
	private int crosstableHeight;
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

			var fitted = Math.Clamp(moveListHeight + (int)slack - BottomPadding, 0, (int)windowHeight);
			if (fitted == moveListHeight) return;

			moveListHeight = fitted;
			await InvokeAsync(StateHasChanged);
			await Task.Delay(50);
		}
	}
}
