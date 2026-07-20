# Chessboard Library Dependencies in EngineBattle WebGUI

> **MIGRATION COMPLETE (July 2026).** Every board in the WebGUI now renders through the
> native Blazor component `ChessboardLayout/EbChessboard.razor` — chessboard2, the
> chessboard-1.0.0 files, and all board-rendering JS in chessInterop.js have been deleted.
> What remains of chessInterop.js: `attachBoardDrag` (pointer tracking for drag & drop),
> Plotly chart helpers, scrolling helpers, and the PGN paste bridge. Piece images live in
> `wwwroot/pieces/wikipedia/*.svg` (cburnett set + LICENSE.md), following the
> `pieces/{set}/` folder convention. This document is kept as the historical dependency
> map that drove the migration, plus the requirements checklist EbChessboard was built
> against (bottom of the page).

## Which library is actually in use?

Only one chessboard library is live (a second one shipped historically):

| Library | Files | Status |
|---|---|---|
| **chessboard2** (chessboard2.oakmac.com) | `wwwroot/js/chessboard2.min.js`, `wwwroot/css/chessboard2.min.css` | **Active.** Loaded globally in `Components/App.razor` (script + stylesheet). All boards are `Chessboard2(...)` instances. |
| chessboardjs 1.0.0 | `wwwroot/chessboardjs/` | **Library removed (July 2026 cleanup).** Only `img/chesspieces/wikipedia/*.png` (used by the pawn-promotion dialog) and `LICENSE.md` (attribution for those images) remain. |

Piece images on the boards themselves are embedded as `data:image` URIs inside
`chessboard2.min.js`, so the board rendering has no dependency on the `chessboardjs/img` folder.

## Architecture

```
Blazor components (.razor)
      │  IJSObjectReference (ESM import "./js/chessInterop.js?v=1.74.1" — bump the
      │  version in JavaScriptInteropService.cs whenever chessInterop.js changes)
      ▼
wwwroot/js/chessInterop.js   ← single interop layer, all board logic lives here
      │  Chessboard2(element, config)
      ▼
wwwroot/js/chessboard2.min.js
```

- The module is imported by `Services/JavaScriptInteropService.cs` (shared singleton — see
  `feedback_chessmodule_singleton`), `MainLayout.razor`, and `DialogChart.razor`.
- **State is stored as expando properties on the board's DOM element**: `element.board`
  (the Chessboard2 instance), `element.arrow2Id`, `element.ponderArrowId`, `element.circleId`,
  `element.circleId2`, `element.arrowIds[]`, `element.circles[]`. A replacement must either
  keep this pattern or introduce a proper instance registry.

## chessboard2 API surface used

A replacement library/component must cover all of this:

| API | Used for |
|---|---|
| `Chessboard2(element, config)` — config: `position`, `showNotation`, `draggable`, `useAnimation`, `onDragStart`, `onDrop` | Board creation |
| `board.position(fen, animate)` | Set position (animated for user moves, instant for live updates) |
| `board.setPosition(fen)` | PV board updates (`setPVBoardWithSquareHighlighting`) |
| `board.resize()` | Responsive layout (window resize, tile grids) |
| `board.flip()` | Rotate board (`rotateBoard`) |
| `board.clear()` | (No longer used — `clearBoard2` removed in cleanup) |
| `board.addArrow({color, start, end, opacity, size})` / `addArrow('e2-e4', color)` → id; `board.removeArrow(id)` | Best-move / ponder / policy / puzzle-solution arrows |
| `board.addCircle({color, square, opacity, size})` → id; `board.removeCircle(id)` | Policy circles, from/to circle highlighting |
| `onDragStart(evt)` → `evt.square`; return `false` to veto | Legality check before drag (async .NET call) |
| `onDrop(evt)` → `evt.source`, `evt.target` | Move entry, promotion detection |
| **DOM contract**: every square addressable as `element.querySelector('[data-square-coord="e4"]')` | Square highlighting, label chips, overlay labels, promotion-dialog positioning, drag opacity tweak |

The last row is the most invasive dependency: last-move highlighting is done by toggling the
custom CSS classes `.highlight-white`/`.highlight-black` (defined in `site_v1.17.css`) directly
on chessboard2's internal square elements, bypassing the library API entirely. The same
`data-square-coord` selector is used to append `.circle-piece` label chips and to position the
overlay layer. A replacement must expose per-square DOM elements or provide equivalent
highlight/annotation APIs.

## chessInterop.js exports — board-related

### Called from .NET

| Function | What it does | Callers |
|---|---|---|
| `createChessboard(element, fen)` | Create board on element; if it already exists, clears highlights, sets position (when fen given) and resizes | PVBoardLive, PVtileBoard, PVboardDuo, StreamingChessboard, MoveDeviation |
| `addChessboardToElement(dotnetHelper, fen, element)` | The full interactive board: draggable pieces, legality veto, promotion flow, from/to highlighting. .NET callbacks: `sideToMove`, `isLegalPieceMove`, `isLegalMove`, `GetMoveStr`, `GetPositionFen`, `UpdateNewMove`, `ShowPromotionDialog` | ModernChessboard |
| `setPosition2(element, fen)` | Set position, clear arrows/highlights | PVtileBoard, StreamingChessboard |
| `setPositionWithCallback(dotnetHelper, element, fen, withCallback)` | Animated position set + optional `UpdateNewMove` callback | ModernChessboard |
| `setSimplePosition(element, fen)` | Instant position set, clear annotations | ModernChessboard |
| `makeSimpleMoveWithCallBack(dotnetHelper, element, color, fromSq, toSq, fen, invokeMove)` | Animated move + from/to highlight + optional callback | ModernChessboard |
| `setBoardWithSquareHighlighting(element, fen, color, fromSq, toSq, withHighLight)` | Position + last-move square highlight; removes arrows/labels | StreamingChessboard, MoveDeviation |
| `setPVBoardWithSquareHighlighting(element, fen, color, fromSq, toSq, withHighLight)` | PV-board variant (uses `setPosition`, auto-creates board) | PVBoardLive, PVboardDuo, PVtileBoard |
| `setArrowHighlighting(element, fromSq, toSq, color)` | Single best-move arrow | StreamingChessboard |
| `setDoubleArrowHighlighting(element, fromSq, toSq, oppFromSq, oppToSq, color, oppColor)` | Both engines' expected moves as two arrows | StreamingChessboard |
| `setPonderArrowHighlightingWithLabel(element, text, fromSq, toSq, arrowColor, textColor, bgColor)` | Ponder arrow + `.circle-piece` label chip on target square | StreamingChessboard |
| `addPolicyCircles(element, moves)` | Policy-head visualization: per-move arrow + circle + overlay label (delegates to `setArrowSequence`) | ModernChessboard |
| `clearOverlayLabels(element)` | Remove all arrows, circles and overlay labels | ModernChessboard |
| `rotateBoard(element)` | `board.flip()` + clear annotations | ModernChessboard |
| `puzzleBoardToElement(element, fen, notation, fromSq, toSq, wFromSq, wToSq)` | Static puzzle board: green solution arrow + crimson wrong-move arrow | EPDVisualization, FenVisualization2 |
| `resizeChessboard2(element)` | `board.resize()` | PVtileBoard, StreamingChessboard |
| `showPromotionDialog(x, y, isWhite)` / `hidePromotionDialog()` | Promotion picker; **loads piece PNGs from `chessboardjs/img/chesspieces/wikipedia/`** (the only live use of the old chessboardjs folder) | ModernChessboard |

### Internal to chessInterop.js only

`ensureChessboard`, `highlightSquare2`, `clearHighlightSquaresForElement`, `setLabel`,
`removeCirclePieces`, `setArrowSequence`, `initializeOverlayLayer`, `addOverlayLabel`.

The overlay layer (`initializeOverlayLayer` / `addOverlayLabel`) is a positioned `div` stacked
on top of the board for policy-percentage labels — it measures square positions via
`getBoundingClientRect()` on the `data-square-coord` elements.

### Removed in the July 2026 cleanup (were exported but unused)

`resizeAllChessboards`, `resizeChessboard`, `setWidthAndHeight`,
`chessBoardToElementWithResizing`, `adjustElement`, `adjustAllPVboards`,
`setCircleHighlighting`, `makeSimpleMove2`, `clearBoard2`.

## Component / page dependency map

### Interactive boards (drag & drop, promotion, policy overlay)

- **`ChessboardLayout/ModernChessboard.razor`** — the primary (and only) interactive board
  component. Used by: **SingleAnalysis**, **DualAnalysis**, **GameReview**, **PlayVsComputer**.
  (The legacy `Chessboard.razor` twin and its two experimental consumer pages, CeresSuite and
  ChessPlayerAnalysis, were deleted in the July 2026 cleanup.)

### Tournament live boards

- **`TournamentLayout/StreamingChessboard.razor`** — main live game board on the
  **Tournaments** page (also live-feed mode). Last-move highlight, engine arrows,
  double arrows, ponder arrow with eval label.
- **`TournamentLayout/PVboardDuo.razor`** — two PV boards (white/black engine) during live games.
- **`TournamentLayout/PVtileBoard.razor`** — minimal PV tile used by **LiveFeedGrid**
  (many boards on one page — replacement must scale to dozens of instances).

### PV / analysis boards

- **`ChessboardLayout/PVBoardLive.razor`** — PV dialog (single/dual mode) opened from
  **EnginePanel** and **DualAnalysis**; navigable PV with square highlighting.

### Static visualization boards

- **`VisualizationLayout/EPDVisualization.razor`** — puzzle/EPD board with solution and
  wrong-move arrows. Used by **TBTestVisualization**, **EpdFileVisualization**,
  **EretPuzzles**, **TBSinglePosition**.
- **`VisualizationLayout/FenVisualization2.razor`** — puzzle solution boards
  (**PuzzleVisualSolution**).
- **`VisualizationLayout/MoveDeviation.razor`** — two boards side by side (played move vs
  deviation). Used by **DeviationFinder** via a **fully qualified tag**
  (`<WebGUI.Components.Layout.VisualizationLayout.MoveDeviation ...>`) — searches for
  `<MoveDeviation` will miss it.

### Dead components (deleted in the July 2026 cleanup)

- **`VisualizationLayout/FenVisualization.razor`** — called JS functions that no longer
  existed and had no references.
- **`ChessboardLayout/SimpleChessboard.razor`**, **`ChessboardLayout/SimplePVBoard.razor`**
  (+ wrapper `PVboards.razor`), **`TournamentLayout/PVboardCarousel.razor`** — no consumers
  anywhere (verified including fully qualified tag names and `typeof`/`DynamicComponent`).

## CSS dependencies

| File | Dependency |
|---|---|
| `wwwroot/css/chessboard2.min.css` | Library styles (loaded in App.razor) |
| `wwwroot/css/site_v1.17.css` | `.highlight-white` / `.highlight-black` (last-move square highlight, applied directly to library square elements), `.circle-piece` (label chips) |
| `ModernChessboard.razor.css`, `SingleAnalysis.razor.css`, `DualAnalysis.razor.css` | `::deep .chessboard-root` sizing rules — `.chessboard-root` is the wrapper div in ModernChessboard markup |
| Inline styles in chessInterop.js | `.overlay-layer`, `.square-overlay` (policy labels), `#promotionDialog` positioning |

## Known bugs / quirks in the current integration

1. ~~`createChessboard` doesn't set the position when the board already exists~~ — **fixed
   July 2026**: the existing-board path had inverted `if (fen)` / `else` branches (with a fen
   it only resized; without one it called `position(undefined)`, a no-op getter). It now
   clears highlights, sets the position when a fen is given, and resizes. PVBoardLive's
   historical workaround (`setPVBoardWithSquareHighlighting` for the start position) still
   works and remains in place.
2. **Highlighting bypasses the library** — CSS classes are toggled on chessboard2's internal
   square elements via `[data-square-coord]`. Any replacement changes this contract.
3. **State on DOM elements** — board instances and annotation IDs live as expando properties
   on elements; disposal is implicit (element removal).
4. Mixed use of `board.position(fen, animate)` and `board.setPosition(fen)` between the
   game-board and PV-board code paths.

## Requirements checklist for a replacement

A replacement (library or hand-rolled Blazor/JS component) must support:

- [ ] Set position from FEN, with and without animation
- [ ] Programmatic resize (responsive layouts, tile grid) and flip
- [ ] Arrows with ids (add/remove individually): best move, ponder, two-engine compare, policy, puzzle solution/wrong move
- [ ] Circles with ids (policy visualization)
- [ ] Square highlighting (last move from/to, drag source) — API-based, not class-hacking
- [ ] Text labels/badges on squares (eval chips, policy percentages)
- [ ] Drag & drop with async veto (legality checked in .NET over SignalR) and drop coordinates
- [ ] Promotion flow hook (currently a custom dialog positioned over the target square)
- [ ] Many simultaneous instances on one page (LiveFeedGrid tiles, PV boards, puzzle grids)
- [ ] Static non-interactive thumbnails (puzzle/EPD boards)
- [ ] Blazor Server friendly: ESM interop, no per-keystroke chatter, cheap updates at ~1 Hz live-PV rate

Customization requirements (new in the replacement — not supported today):

- [ ] Piece sets as a folder convention (`wwwroot/pieces/{set}/wK.svg` … `bP.svg`, SVG) with a
      `PieceSet` parameter — users add their own set by dropping in a folder, no code changes.
      Bundle 3-4 permissively licensed sets (cburnett matches the current default look).
- [ ] Board theming via CSS custom properties on the board root (`--eb-light-square`,
      `--eb-dark-square`, `--eb-highlight`, `--eb-arrow`, …) with presets + custom colors.
      Highlights/circles must be semi-transparent overlays so they work on any theme.
- [ ] Coordinates (a1-h8) as in-square edge labels, following board orientation on flip,
      auto-hidden below a size threshold (small live-feed tiles).
- [ ] One ThemeService persisted in GlobalSettings, editable on the Settings page with live
      preview; all boards on a page follow the theme automatically.
- [ ] Size-adaptive rendering: arrow stroke width, label chips, and badges scale with board
      size; animation is a user toggle (also a perf knob with many boards).
- [ ] Board frame/background follows the MudBlazor light/dark theme (square colors follow the
      board theme).

Cleanup status (July 2026, commits `965053d` and `7dc7d60`): the unused chessboardjs 1.0.0
library files, the dead `FenVisualization.razor` component, the unused interop exports, the
legacy `Chessboard.razor` component, the experimental CeresSuite/ChessPlayerAnalysis pages,
and the four unused board components (SimpleChessboard, SimplePVBoard, PVboards,
PVboardCarousel) have all been removed. Only the promotion-dialog piece images and their
LICENSE.md remain under `wwwroot/chessboardjs/`.
