
//write to clipboard
export async function writeToClipboard(text) {
  await navigator.clipboard.writeText(text);
}

export async function readTextFromClipboard() {
  return await navigator.clipboard.readText();
}

// Function to get the current window width
export function getWindowWidth() {
  return window.innerWidth;
}

export function getWindowHeight() {
  return window.innerHeight;
}

export function getFullScreenSize() {
  return {
    width: window.screen.width,
    height: window.screen.height
  };
}

// returns the *viewport* size (excludes browser chrome, OS taskbars, etc.)
export function getViewportSize() {
    return {
        width: window.innerWidth,
        height: window.innerHeight
    };
}

export function getUnzoomedViewportHeight() {
    // visualViewport gives you both height (in CSS px) and the current zoom scale
    const zoom = window.screen.width / window.innerWidth;
    return window.innerHeight * zoom;
}

// Every measurement the tournament page needs, in one call. On Blazor Server each interop
// call is a round-trip over the SignalR circuit, and resize events arrive in bursts, so taking
// these one at a time cost six round-trips per resize.
export function measureTournamentLayout() {
  const sum = (className) => {
    const els = document.getElementsByClassName(className);
    let total = 0;
    for (let i = 0; i < els.length; i++) total += els[i].offsetHeight;
    return total;
  };

  // The engine panel's width. The two PV boards are cells of that panel's grid, so this is
  // what they have to share, and C# turns it into a pixel size for each one - which is how
  // the main board's size slider reaches them: it widens this column, this number grows.
  // Measured on the panel rather than on the boards so it is still there when they are off.
  const pvRow = () => {
    const el = document.querySelector('.engine-stats-fit');
    return el ? Math.round(el.clientWidth) : 0;
  };

  return {
    windowHeight: window.innerHeight,
    unzoomedViewportHeight: window.innerHeight * (window.screen.width / window.innerWidth),
    lhs: sum('lhs'),
    rhs: sum('rhs'),
    standingTable: sum('standingTable'),
    pvRow: pvRow()
  };
}

// A control that is invisible until the pointer comes for it, so it never sits on top of what
// the page draws in that corner. A class on <html> rather than styling the element from here,
// so the look stays in CSS; while it is hidden the control also takes no clicks, which a plain
// opacity:0 would not give us. Registering the same name twice is a no-op, so a component may
// call this on every render.
//
// The region is the CONTROL'S OWN BOX grown by a margin, not a guessed radius from the corner.
// A fixed radius only works while the control is smaller than it: a control two rows tall and
// 200px wide reaches outside the region it is summoned by, so moving onto its far button takes
// the pointer out of range and the thing vanishes under the cursor. The element is laid out
// even while it is transparent, so its rect is always the truth.
export function watchCorner(name, selector, marginPx) {
  window.__ebCorners = window.__ebCorners || {};
  if (window.__ebCorners[name]) return;
  window.__ebCorners[name] = true;
  const m = marginPx || 60;
  const cls = 'eb-near-' + name;
  const near = function (e) {
    const el = document.querySelector(selector);
    if (!el) return false;               // not rendered on this page, or not right now
    const r = el.getBoundingClientRect();
    if (!r.width && !r.height) return false;
    return e.clientX >= r.left - m && e.clientX <= r.right + m
        && e.clientY >= r.top - m && e.clientY <= r.bottom + m;
  };
  document.addEventListener('pointermove', function (e) {
    document.documentElement.classList.toggle(cls, near(e));
  }, { passive: true });
  // A pointer that leaves the window entirely should not leave the control showing.
  document.addEventListener('pointerleave', function () {
    document.documentElement.classList.remove(cls);
  }, { passive: true });
}

// How much wider the content of a box is than the box itself, and how wide the box is. Used
// where the width depends on content no formula predicts: the crosstable, whose column count
// and header text grow together, and the info banner, whose hardware line is whatever the user
// typed.
//
// `ratio` is the WORST of all matches, because a banner is two rows and the wider one decides.
// It is never below 1: scrollWidth cannot be smaller than clientWidth, so a box can report
// that it is too small but never that it has room to spare. `box` is what makes growing back
// possible at all - when it changes, the caller knows to start again from the full size
// instead of dividing a number it can only ever make smaller.
export function measureOverflow(selector) {
  const els = document.querySelectorAll(selector);
  let ratio = 1;
  let box = 0;
  for (const el of els) {
    if (!el.clientWidth) continue;
    const r = el.scrollWidth / el.clientWidth;
    if (r > ratio) ratio = r;
    if (!box || el.clientWidth < box) box = el.clientWidth;
  }
  return { ratio: ratio, box: box };
}

// The font size each of these selectors ACTUALLY ended up with, rounded to whole pixels. This
// is what "save these sizes" writes to tournament.json: the ceiling, the nudge and the clamp
// have all already been applied, so the file records what is on screen rather than an input.
export function getComputedFontSizes(selectors) {
  const out = {};
  for (const key of Object.keys(selectors || {})) {
    const el = document.querySelector(selectors[key]);
    if (!el) continue;
    const px = parseFloat(getComputedStyle(el).fontSize);
    if (px > 0) out[key] = Math.round(px);
  }
  return out;
}

// Which screen this browser is on, as a key the settings file can store a font scale under.
// Physical pixels and the OS scaling both matter: 1920x1080 at 150% is a different reading
// distance from 1920x1080 at 100%.
export function getScreenBucket() {
  try {
    const dpr = Math.round((window.devicePixelRatio || 1) * 100) / 100;
    return screen.width + 'x' + screen.height + '@' + dpr;
  } catch (e) {
    return '';
  }
}

// Unused space between the bottom of an element and the bottom of the viewport.
// Positive means there is slack left over, negative means the element has been pushed past
// the bottom edge. Callers use it to size an elastic region from what the layout actually
// did, instead of predicting it from part heights plus tuned constants.
// Returns 0 when the element is absent, which callers treat as "nothing to do".
// The box a region has, and the content it wants to put in it, for every selector named - one
// round trip rather than one per box. clientHeight is what the cap left it; scrollHeight is what
// the table would be if nothing capped it. A caller sharing one column between two boxes needs
// both numbers for both boxes before it can decide anything.
// Rest parameters, not an array parameter: InvokeAsync takes params object[], and a string[]
// IS an object[], so an array argument arrives here spread into separate arguments rather than
// as one list. This signature is the one that matches how the call site can actually send it.
export function measureBoxHeights(...selectors) {
  return selectors.map(sel => {
    const el = document.querySelector(sel);
    return el
      ? { found: true, box: el.clientHeight, content: el.scrollHeight }
      : { found: false, box: 0, content: 0 };
  });
}

// How much room is left under a region, for a caller that grows that region into it.
//
// The LOWEST match, not the first. A selector can name several boxes, and then the free space
// is the space under the bottom one: the standings column may carry a crosstable below the
// standings box, and measuring the standings alone counted the crosstable's own height as free
// room, so the box grew by it and pushed the crosstable out under the window edge. Where only
// one box matches - every other caller, and the standings column in every other layout - this
// is the same measurement as before.
//
// Hidden boxes are skipped rather than allowed to win with a zero rect, and no visible match
// returns 0, which callers read as "nothing to fit on this layout" and leave the region alone.
export function getSlackBelow(selector) {
  let bottom = null;
  for (const el of document.querySelectorAll(selector)) {
    const r = el.getBoundingClientRect();
    if (!r.width && !r.height) continue;
    if (bottom === null || r.bottom > bottom) bottom = r.bottom;
  }
  if (bottom === null) return 0;
  return window.innerHeight - bottom;
}

//function to calculate the height of all elements with the given class name
export function calculateHeightByClassName(className) {
  var elements = document.getElementsByClassName(className);
  var height = 0;
  for (var i = 0; i < elements.length; i++) {
    height += elements[i].offsetHeight;
  }
  return height;
}

//function to calculate the height of all elements under a given div element
export function calculateHeightByElementId(id) {
  var element = document.getElementById(id);
  var height = 0;
  if (element) {
    var children = element.children;
    for (var i = 0; i < children.length; i++) {
      height += children[i].offsetHeight;
    }
  }
  return height;
}

// Single-slot resize hook: registering replaces any previous listener, and the page
// unregisters on dispose. The old anonymous-listener version accumulated one permanent
// listener (pinning its DotNetObjectReference) per page visit in the same tab.
let resizeHandler = null;
export function registerResizeEvent(dotnetReference) {
  unregisterResizeEvent();
  resizeHandler = () => {
    dotnetReference.invokeMethodAsync('OnBrowserResize');
  };
  window.addEventListener("resize", resizeHandler);
}

export function unregisterResizeEvent() {
  if (resizeHandler) {
    window.removeEventListener("resize", resizeHandler);
    resizeHandler = null;
  }
}

// This function triggers the resize event on the window.
export function triggerResizeEvent() {
  window.dispatchEvent(new Event('resize'));
}

// ── Native board drag & drop (EbChessboard) ─────────────────────────────
// Pointer tracking only: a ghost image follows the pointer; all chess logic
// (legality, promotion) stays in .NET. Drop reports fromSq/toSq via dotnetRef.
export function attachBoardDrag(container, dotnetRef) {
  let drag = null;

  function squareAt(x, y) {
    const el = document.elementFromPoint(x, y);
    const sq = el && el.closest ? el.closest('[data-eb-sq]') : null;
    return sq ? sq.getAttribute('data-eb-sq') : null;
  }

  function moveGhost(x, y) {
    if (!drag) return;
    drag.ghost.style.transform = `translate(${x - drag.w / 2}px, ${y - drag.h / 2}px)`;
  }

  function onPointerMove(ev) {
    moveGhost(ev.clientX, ev.clientY);
  }

  function cleanup() {
    if (!drag) return;
    drag.ghost.remove();
    if (drag.pieceEl.isConnected) drag.pieceEl.style.opacity = '';
    window.removeEventListener('pointermove', onPointerMove, true);
    window.removeEventListener('pointerup', onPointerUp, true);
    window.removeEventListener('pointercancel', cleanup, true);
    drag = null;
    // Every drag ending funnels through here (drop, cancel, refused source, dispose):
    // tell .NET so transient UI like the safe-square preview can clear.
    try { dotnetRef.invokeMethodAsync('OnBoardDragEnd').catch(() => { }); } catch { }
  }

  function onPointerUp(ev) {
    if (!drag) return;
    const fromSq = drag.fromSq;
    cleanup();
    const toSq = squareAt(ev.clientX, ev.clientY);
    if (toSq && toSq !== fromSq) {
      dotnetRef.invokeMethodAsync('OnBoardDragDrop', fromSq, toSq);
    }
  }

  function onPointerDown(ev) {
    if (drag || (ev.button !== undefined && ev.button !== 0)) return;
    const pieceEl = ev.target && ev.target.closest ? ev.target.closest('.eb-piece') : null;
    if (!pieceEl) return;
    const squareEl = pieceEl.closest('[data-eb-sq]');
    if (!squareEl) return;
    const fromSq = squareEl.getAttribute('data-eb-sq');
    ev.preventDefault();

    const rect = pieceEl.getBoundingClientRect();
    const ghost = document.createElement('img');
    ghost.src = pieceEl.src;
    ghost.style.cssText =
      `position:fixed; left:0; top:0; width:${rect.width}px; height:${rect.height}px; ` +
      'pointer-events:none; z-index:10000; opacity:0.9; will-change:transform;';
    document.body.appendChild(ghost);
    // Hide the source piece completely while dragging (pre-migration chessboard2 behavior)
    pieceEl.style.opacity = '0';

    drag = { fromSq, ghost, pieceEl, w: rect.width, h: rect.height };
    moveGhost(ev.clientX, ev.clientY);

    // Legality of the drag source is decided in .NET; cancel the drag if refused.
    dotnetRef.invokeMethodAsync('CanDragFrom', fromSq)
      .then(ok => { if (!ok) cleanup(); })
      .catch(() => cleanup());

    // Capture-phase listeners on window: nothing on the page can swallow the events.
    window.addEventListener('pointermove', onPointerMove, true);
    window.addEventListener('pointerup', onPointerUp, true);
    window.addEventListener('pointercancel', cleanup, true);
  }

  container.addEventListener('pointerdown', onPointerDown);
  // Belt and braces: never let native HTML5 image drag hijack the gesture.
  container.addEventListener('dragstart', ev => ev.preventDefault());

  return {
    dispose: () => {
      cleanup();
      container.removeEventListener('pointerdown', onPointerDown);
    }
  };
}

// ── Plotly charts ───────────────────────────────────────────────────────
export function setLineChartData(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2, config.trace3];
    Plotly.newPlot(chart, data, layout, { displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

// Trend charts have one trace per training arm/variant, so the count is not known up front
// (the fixed-arity helpers above take trace1..trace3). Pass a plain array instead.
export function setMultiTraceChart(chart, traces, layout) {
  try {
    Plotly.newPlot(chart, traces, layout, { responsive: true, displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function setSingleNodeChart(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function setDoubleNodeChart(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function setLineEvalChartData(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function updateLineEvalChartData(chart, trace, index) {
  try {
    var x = trace.x;
    var y = trace.y;
    var x0 = [[x[y.length - 1]]];
    var y0 = [[y[y.length - 1]]];
    Plotly.extendTraces(chart, { x: x0, y: y0 }, [index]);
    var lastX = x[y.length - 1];
    var firstX = x[0];
    var span = lastX - firstX + 2;
    var raw = span / 10;
    var nice = [1, 2, 5, 10, 20, 50, 100];
    var dtick = nice.find(n => n >= raw) || nice[nice.length - 1];
    if (chart.layout.xaxis.dtick !== dtick)
      Plotly.relayout(chart, { 'xaxis.dtick': dtick });
  } catch (error) {
    console.error(error);
  }
}

export function setTimeUsageChartData(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function resizePlot(chart) {
  try {
    Plotly.Plots.resize(chart);
  } catch (error) {
    console.error(error);
  }
}

// newPlot, not react: react merges into the live _fullLayout instead of rebuilding it, so a
// search that started with two visited moves kept showing two as the tree opened up, and an
// axis range from an early draw outlived the layout passed in.
export function setQdataPlot(chart, layout, arr) {
  var data = arr;
  Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
}

export function setNdataPlot(chart, layout, data) {
  Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
}

export function updateQdataPlot(chart, trace, index) {
  try {
    var x = trace.x;
    var y = trace.y;
    var x0 = [[x[x.length - 1]]];
    var y0 = [[y[y.length - 1]]];
    Plotly.extendTraces(chart, { x: x0, y: y0 }, [index]);
  } catch (error) {
    console.error(error);
  }
}

export function clearQPlot(chart, layout) {
  Plotly.react(chart, [], layout, { responsive: true, displayModeBar: false });
}

export function setPgnEvalPlot(dotnetHelper, chart, layout, data) {

  Plotly.newPlot(chart, data, layout, { responsive: true });
  chart.on('plotly_click', function (data) {
    var pts = '';
    for (var i = 0; i < data.points.length; i++) {
      pts = data.points[i].x +
        ',' + data.points[i].y.toPrecision(4);
    }
    dotnetHelper.invokeMethodAsync('UpdatePlotInfo', pts);
  });
}

export function changeColorInEvalPlot(chart, data) {
  var X = [data.x - 1];
  var Y = [data.y];

  var l = chart.data.length;
  if (l > 2) {
    Plotly.deleteTraces(chart, 2);
  }

  Plotly.addTraces(chart, {
    x: X,
    y: Y,
    type: 'bar',
    mode: 'markers',
    marker: { 'color': 'red' },
    showlegend: false,
  });
}

export function updateMoveIndicator(div, moveIndex, color = '#FFD400') {
    if (!div || !div.layout) return;

    const lineShape = {
        type: 'line',
        xref: 'x',
        yref: 'paper',
        x0: moveIndex,
        x1: moveIndex,
        y0: 0,
        y1: 1,
        line: { color: color, width: 2 },
        name: 'moveIndicator'
    };

    const existing = (div.layout.shapes || []).filter(s => s.name !== 'moveIndicator');
    existing.push(lineShape);

    Plotly.relayout(div, { shapes: existing });
}

// ── Scrolling helpers ───────────────────────────────────────────────────
// Scroll within the container using rect deltas (works with inline spans + wrapping/comments).
function centerInContainer(container, el) {
    const cRect = container.getBoundingClientRect();
    const eRect = el.getBoundingClientRect();
    const delta = (eRect.top - cRect.top) - (container.clientHeight / 2) + (eRect.height / 2);
    container.scrollTop += delta;
}

// The move the list is meant to be showing, per container, plus the observer watching that
// container. A shorter container shows an earlier part of the list at the same scrollTop, so
// anything that resizes it — the candidate-move panel appearing above when a search starts,
// a window resize, a font change — silently scrolls the list away from the current move.
// Re-applying the remembered target on resize covers all of them at the source.
const moveListTargets = new Map();
const moveListObservers = new Map();

function watchMoveListResize(containerId) {
    if (moveListObservers.has(containerId)) {
        const [observed, obs] = moveListObservers.get(containerId);
        if (observed.isConnected) return;
        obs.disconnect();
    }
    const container = document.getElementById(containerId);
    if (!container || typeof ResizeObserver === "undefined") return;
    const obs = new ResizeObserver(() => {
        const id = moveListTargets.get(containerId);
        const c = document.getElementById(containerId);
        const el = id ? document.getElementById(id) : null;
        if (c && el) centerInContainer(c, el);
    });
    obs.observe(container);
    moveListObservers.set(containerId, [container, obs]);
}

// Returns whether the move was found and scrolled to. A miss means the row has not been
// rendered yet — the caller keeps its request pending and retries after the next render.
// It must not reposition the list on a miss: scrolling to the top was worse than doing
// nothing, since the caller then had no way to tell the two apart.
export function scrollToMoveListElement(containerId, elementId) {
    const container = document.getElementById(containerId);
    if (!container) return false;

    const el = document.getElementById(elementId);
    if (!el) return false;

    centerInContainer(container, el);
    moveListTargets.set(containerId, elementId);
    watchMoveListResize(containerId);
    return true;
}

export function scrollToEnd(textarea) {
  textarea.scrollTop = textarea.scrollHeight;
}

export function scrollDivToTop(div) {
    var element = document.getElementById(div);
    if (element) {
        element.scrollTop = 0; // Scroll the div element to the top
    }
}

export function scrollDivToEnd(div) {
  var element = document.getElementById(div);
  // Scroll the div element to the last child element
    if (element) {
        element.scrollTop = element.scrollHeight;
    }
}

export function scrollToElement(containerId, elementId) {
    const container = document.getElementById(containerId);
    const element = document.getElementById(elementId);

    if (container && element) {
        // Calculate the scroll position to center the element
        const scrollTop = element.offsetTop - container.offsetTop - (container.clientHeight / 2) + (element.offsetHeight / 2);

        // Smooth scroll to the position
        container.scrollTo({
            top: scrollTop,
            behavior: 'smooth'
        });
    }
}

  // --- PGN paste bridge (keeps browser "user activation" by reading from the paste event) ---
  let _pgnPasteDotnet = null;
  let _pgnKeydownHandler = null;
  let _pgnPasteHandler = null;
  let _pgnPasteTarget = null;

  function _isEditableElement(el) {
    if (!el) return false;
    const tag = (el.tagName || '').toUpperCase();
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') {
      return !(el.readOnly || el.disabled);
    }
    return !!el.isContentEditable;
  }

  function _ensurePgnPasteTarget() {
    if (_pgnPasteTarget) return _pgnPasteTarget;
    const ta = document.createElement('textarea');
    ta.id = 'pgnPasteTarget';
    ta.setAttribute('aria-hidden', 'true');
    ta.tabIndex = -1;
    ta.style.position = 'fixed';
    ta.style.left = '-10000px';
    ta.style.top = '0';
    ta.style.width = '1px';
    ta.style.height = '1px';
    ta.style.opacity = '0';
    // Keep focusable via JS, but don't interfere with mouse.
    ta.style.pointerEvents = 'none';
    document.body.appendChild(ta);
    _pgnPasteTarget = ta;
    return ta;
  }

  async function _sendPgnToDotNet(text) {
    if (!_pgnPasteDotnet) return;

    const trimmed = (text || '').trim();
    if (!trimmed) return;

    // Keep chunks comfortably below typical Blazor Server / SignalR message limits.
    const CHUNK_SIZE = 12000;

    try {
      if (trimmed.length <= CHUNK_SIZE) {
        await _pgnPasteDotnet.invokeMethodAsync('OnPgnPasted', trimmed);
        return;
      }

      const id = `${Date.now()}-${Math.random().toString(16).slice(2)}`;
      const totalChunks = Math.ceil(trimmed.length / CHUNK_SIZE);
      await _pgnPasteDotnet.invokeMethodAsync('OnPgnPasteBegin', id, totalChunks);
      for (let i = 0; i < totalChunks; i++) {
        const chunk = trimmed.slice(i * CHUNK_SIZE, (i + 1) * CHUNK_SIZE);
        await _pgnPasteDotnet.invokeMethodAsync('OnPgnPasteChunk', id, i, chunk);
      }
      await _pgnPasteDotnet.invokeMethodAsync('OnPgnPasteEnd', id);
    } catch (err) {
      console.error('PGN paste -> .NET failed', err);
    }
  }

  export function registerGlobalPgnPaste(dotnetHelper) {
    // Always keep the latest helper (page refresh/reconnect).
    _pgnPasteDotnet = dotnetHelper;

    const ta = _ensurePgnPasteTarget();

    if (!_pgnPasteHandler) {
      _pgnPasteHandler = async (ev) => {
        try {
          const text = ev.clipboardData ? ev.clipboardData.getData('text/plain') : '';
          if (text && text.trim().length > 0) {
            // Consume the paste so it doesn't end up in some random control.
            ev.preventDefault();
            await _sendPgnToDotNet(text);
          }
        } finally {
          // Always clear the hidden textarea.
          ta.value = '';
        }
      };
      ta.addEventListener('paste', _pgnPasteHandler);
    }

    if (!_pgnKeydownHandler) {
      _pgnKeydownHandler = (ev) => {
        const isV = ev.key === 'v' || ev.key === 'V';
        if (!isV) return;

        const hasModifier = ev.ctrlKey || ev.metaKey;
        if (!hasModifier || ev.altKey) return;

        // If the user is editing text, do not steal their paste.
        if (_isEditableElement(document.activeElement)) return;

        // Focus our hidden target so the browser dispatches a paste event we can read from.
        _ensurePgnPasteTarget().focus({ preventScroll: true });
        // Do NOT preventDefault here; we want the paste event to proceed.
      };
      // Capture so we run before other key handlers that might stop propagation.
      document.addEventListener('keydown', _pgnKeydownHandler, true);
    }
  }

  export function unregisterGlobalPgnPaste() {
    if (_pgnKeydownHandler) {
      document.removeEventListener('keydown', _pgnKeydownHandler, true);
      _pgnKeydownHandler = null;
    }
    if (_pgnPasteTarget && _pgnPasteHandler) {
      _pgnPasteTarget.removeEventListener('paste', _pgnPasteHandler);
      _pgnPasteHandler = null;
    }
    _pgnPasteDotnet = null;
  }
