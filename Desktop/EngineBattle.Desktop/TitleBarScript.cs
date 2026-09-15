namespace EngineBattle.Desktop;

/// <summary>
/// Injected into every document at creation time, turning EngineBattle's own MudBlazor app bar
/// into the window caption.
///
/// This lives in the shell rather than in WebGUI on purpose: the web app stays completely
/// unchanged, and a normal browser session can never render window buttons that do nothing.
/// The only coupling is the <c>header.mud-appbar</c> selector; if that ever changes, the
/// buttons still appear (they are positioned against the viewport) and only dragging by the
/// header is lost.
/// </summary>
internal static class TitleBarScript
{
    public const string Script = """
        (function () {
          try {
            if (window.__ebShellTitleBar) return;
            window.__ebShellTitleBar = true;

            // Lets page code detect that it is hosted by the desktop shell.
            window.engineBattleShell = { version: 1 };

            var post = function (action) {
                try { window.chrome.webview.postMessage({ ebWindow: action }); } catch (e) { }
            };

            var style = document.createElement('style');
            style.textContent = [
                '#eb-window-controls { position: fixed; top: 0; right: 0; height: 40px;',
                '  display: flex; z-index: 2147483000; -webkit-user-select: none; user-select: none; }',
                '#eb-window-controls button { width: 46px; height: 40px; border: 0; padding: 0;',
                '  background: transparent; color: #e8eaf0; cursor: default; line-height: 1;',
                '  display: flex; align-items: center; justify-content: center; }',
                '#eb-window-controls svg { width: 12px; height: 12px; stroke: currentColor;',
                '  fill: none; stroke-width: 1; shape-rendering: crispEdges; }',
                '#eb-window-controls button:hover { background: rgba(255,255,255,0.09); }',
                '#eb-window-controls button.eb-close:hover { background: #c42b1c; color: #fff; }',
                '#eb-window-controls button.eb-output { font-size: 13px; }',
                'header.mud-appbar { -webkit-user-select: none; user-select: none; }',
                /* Fullscreen hides the window buttons, exactly as a browser hides its chrome
                   on F11. F11 or Esc brings them back. */
                'html.eb-fullscreen #eb-window-controls { display: none; }',
                'html.eb-fullscreen header.mud-appbar .mud-toolbar { padding-right: 0 !important; }',
                /* Keep the app bar's own trailing content clear of the buttons. */
                'header.mud-appbar .mud-toolbar { padding-right: 200px !important; }'
            ].join('\n');

            var addStyle = function () { (document.head || document.documentElement).appendChild(style); };
            if (document.head) { addStyle(); } else { document.addEventListener('DOMContentLoaded', addStyle); }

            // Inline SVG rather than the Segoe icon font: WebView2 does not reliably resolve
            // "Segoe Fluent Icons", and a missing glyph on a transparent button is invisible
            // rather than obviously broken.
            var svg = function (inner) {
                return '<svg viewBox="0 0 12 12" aria-hidden="true">' + inner + '</svg>';
            };
            var ICON_OUTPUT = svg('<path d="M1.5 2.5l3 2.5-3 2.5"/><path d="M6 9.5h4.5"/>');
            var ICON_MIN = svg('<path d="M1.5 6.5h9"/>');
            var ICON_MAX = svg('<rect x="1.5" y="1.5" width="9" height="9"/>');
            var ICON_RESTORE = svg('<rect x="1.5" y="3.5" width="7" height="7"/><path d="M3.5 3.5v-2h7v7h-2"/>');
            var ICON_CLOSE = svg('<path d="M1.5 1.5l9 9"/><path d="M10.5 1.5l-9 9"/>');

            var build = function () {
                if (!document.body || document.getElementById('eb-window-controls')) return;
                var host = document.createElement('div');
                host.id = 'eb-window-controls';
                host.innerHTML =
                    '<button class="eb-output" title="Server output (Ctrl+Shift+L)">' + ICON_OUTPUT + '</button>' +
                    '<button class="eb-min" title="Minimise">' + ICON_MIN + '</button>' +
                    '<button class="eb-max" title="Maximise">' + ICON_MAX + '</button>' +
                    '<button class="eb-close" title="Close">' + ICON_CLOSE + '</button>';
                document.body.appendChild(host);
                host.querySelector('.eb-output').onclick = function () { post('output'); };
                host.querySelector('.eb-min').onclick = function () { post('minimize'); };
                host.querySelector('.eb-max').onclick = function () { post('toggleMaximize'); };
                host.querySelector('.eb-close').onclick = function () { post('close'); };
            };

            if (document.readyState === 'loading') {
                document.addEventListener('DOMContentLoaded', build);
            } else {
                build();
            }

            // Blazor replaces large parts of the DOM on navigation; re-add if we get removed.
            var observer = new MutationObserver(build);
            var observe = function () {
                if (document.body) observer.observe(document.body, { childList: true });
            };
            if (document.body) { observe(); } else { document.addEventListener('DOMContentLoaded', observe); }

            // Fullscreen reclaims the navigation rail for the board, the way EngineBattle's own
            // full-screen layout mode does. Driven from here rather than from WebGUI so the web
            // app stays unaware of the shell. Only MudBlazor's own classes are touched, and if
            // they ever change this degrades to doing nothing.
            var wasFullscreen = false;
            var drawerClosedByShell = false;

            var drawerIsOpen = function () {
                var d = document.querySelector('.mud-drawer');
                return !!(d && d.classList.contains('mud-drawer--open'));
            };

            var toggleDrawer = function () {
                var btn = document.querySelector('header.mud-appbar .mud-toolbar button');
                if (btn) btn.click();
            };

            var syncDrawerToFullscreen = function (fullscreen) {
                if (fullscreen === wasFullscreen) return false;
                wasFullscreen = fullscreen;

                if (fullscreen) {
                    if (!drawerIsOpen()) return false;
                    toggleDrawer();
                    // Remembered, so a drawer the user had already closed is not forced open
                    // again on the way out of fullscreen.
                    drawerClosedByShell = true;
                    return true;
                }

                if (!drawerClosedByShell) return false;
                drawerClosedByShell = false;
                if (drawerIsOpen()) return false;
                toggleDrawer();
                return true;
            };

            var isInteractive = function (el) {
                return !!(el && el.closest && el.closest(
                    'button, a, input, select, textarea, [role="button"], .mud-icon-button, #eb-window-controls'));
            };

            var onAppBar = function (el) {
                return !!(el && el.closest && el.closest('header.mud-appbar'));
            };

            // Dragging cannot use WindowChrome's caption area: the WebView2 is an HwndHost and
            // takes the mouse input first, so the host window never hit-tests it. Instead the
            // page reports the gesture and the shell starts a native move loop.
            document.addEventListener('pointerdown', function (e) {
                if (e.button !== 0) return;
                if (!onAppBar(e.target) || isInteractive(e.target)) return;
                post('drag');
            }, true);

            document.addEventListener('dblclick', function (e) {
                if (!onAppBar(e.target) || isInteractive(e.target)) return;
                post('toggleMaximize');
            }, true);

            document.addEventListener('keydown', function (e) {
                if (e.ctrlKey && e.shiftKey && (e.key === 'L' || e.key === 'l')) {
                    e.preventDefault();
                    post('output');
                }
                // Browser accelerator keys are disabled so they cannot steal EngineBattle's own
                // shortcuts, so F11/F12 have to be routed explicitly.
                if (e.key === 'F12' || (e.ctrlKey && e.shiftKey && (e.key === 'I' || e.key === 'i'))) {
                    e.preventDefault();
                    post('devtools');
                }
                if (e.key === 'F11') {
                    e.preventDefault();
                    post('toggleFullScreen');
                }
                if (e.key === 'Escape' && document.documentElement.classList.contains('eb-fullscreen')) {
                    e.preventDefault();
                    post('toggleFullScreen');
                }
            }, true);

            // The shell reports maximise state so the glyph matches.
            try {
                window.chrome.webview.addEventListener('message', function (ev) {
                    var data = ev.data;
                    if (!data || !data.ebWindowState) return;
                    var fullscreen = data.ebWindowState === 'fullscreen';
                    var maximised = data.ebWindowState === 'maximized';
                    var btn = document.querySelector('#eb-window-controls .eb-max');
                    if (btn) {
                        btn.innerHTML = maximised ? ICON_RESTORE : ICON_MAX;
                        btn.title = maximised ? 'Restore' : 'Maximise';
                    }
                    document.documentElement.classList.toggle('eb-maximized', maximised);
                    document.documentElement.classList.toggle('eb-fullscreen', fullscreen);

                    var drawerMoved = syncDrawerToFullscreen(fullscreen);

                    // The viewport changed height without a window resize event in some
                    // transitions; nudge listeners that size themselves from innerHeight.
                    window.dispatchEvent(new Event('resize'));

                    // The drawer slides for 225ms and changes the width the board lays out in,
                    // so re-measure once it has settled. The page debounces, so this is cheap.
                    if (drawerMoved) {
                        setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 350);
                    }
                });
            } catch (e) { }
          } catch (err) {
            window.__ebTitleBarError = (err && err.stack) ? err.stack : String(err);
          }
        })();
        """;
}
