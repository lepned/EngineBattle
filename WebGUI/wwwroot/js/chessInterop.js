
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

export function registerResizeEvent(dotnetReference) {
  window.addEventListener("resize", () => {
    dotnetReference.invokeMethodAsync('OnBrowserResize');
  });
}

// This function triggers the resize event on the window.
export function triggerResizeEvent() {
  window.dispatchEvent(new Event('resize'));
}

export function openNewWindowAndWriteContent(content) {
  var newWindow = window.open("", "_blank");
  newWindow.document.write('<pre>' + content + '</pre>');
  newWindow.document.close();
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

// ── Misc window / scrolling helpers ─────────────────────────────────────
export function openBrowserWindow(content) {
  var doc = window.open().document;
  doc.write("<pre>");
  doc.write(content);
  doc.write(" </pre>");
}

export function scrollToMoveListElement(containerId, elementId) {
    const container = document.getElementById(containerId);
    if (!container) return;

    const el = document.getElementById(elementId);
    if (!el) {
        // If we can't find the move element (e.g. at root position), fall back to top.
        container.scrollTop = 0;
        return;
    }

    // Scroll within the container using rect deltas (works with inline spans + wrapping/comments).
    const cRect = container.getBoundingClientRect();
    const eRect = el.getBoundingClientRect();

    const delta = (eRect.top - cRect.top) - (container.clientHeight / 2) + (eRect.height / 2);
    container.scrollTop += delta;
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
