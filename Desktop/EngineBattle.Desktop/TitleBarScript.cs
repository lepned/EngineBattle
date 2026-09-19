namespace EngineBattle.Desktop;

/// <summary>
/// Injected into every document at creation time. It draws nothing of its own: the window has
/// an ordinary Windows title bar and a menu, so what is left for the page is what only the page
/// can do - route the shortcuts (keys pressed inside the WebView never reach WPF), click the
/// drawer toggle when the shell's menu asks for it, and hide that toggle while the menu is
/// carrying one.
///
/// This lives in the shell rather than in WebGUI on purpose: the web app stays completely
/// unchanged, and a normal browser session is not affected at all. The only coupling is the
/// toggle's own class; if that changes, the menu's hamburger stops working and nothing else.
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

            // The menu strip carries the toggle, so the page's own is never needed here - and
            // fullscreen drops the chrome entirely. The drawer keeps the state it had on the way
            // in, so navigation is still there if it was open, and F11 brings the menu back.
            var style = document.createElement('style');
            style.textContent = '.eb-drawer-toggle { display: none; }';
            var addStyle = function () { (document.head || document.documentElement).appendChild(style); };
            if (document.head) { addStyle(); } else { document.addEventListener('DOMContentLoaded', addStyle); }

            var toggleDrawer = function () {
                // Either button toggles it; only one of them is rendered at a time.
                var btn = document.querySelector('.eb-drawer-toggle, .eb-drawer-close');
                if (btn) btn.click();
            };

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
                // The View menu's accelerators. Ctrl+= is the unshifted '+' key, and the numpad
                // sends '+'/'-'/'0' as the key itself.
                if (e.ctrlKey && !e.altKey && (e.key === '+' || e.key === '=')) {
                    e.preventDefault();
                    post('zoomIn');
                }
                if (e.ctrlKey && !e.altKey && e.key === '-') {
                    e.preventDefault();
                    post('zoomOut');
                }
                if (e.ctrlKey && !e.altKey && e.key === '0') {
                    e.preventDefault();
                    post('zoomReset');
                }
                if (e.key === 'F5') {
                    e.preventDefault();
                    post('reload');
                }
            }, true);

            // The shell reports the window state; Escape reads eb-fullscreen to leave it.
            try {
                window.chrome.webview.addEventListener('message', function (ev) {
                    var data = ev.data;
                    if (!data) return;
                    if (data.ebCommand === 'toggleDrawer') { toggleDrawer(); return; }
                    if (!data.ebWindowState) return;
                    var fullscreen = data.ebWindowState === 'fullscreen';
                    document.documentElement.classList.toggle('eb-fullscreen', fullscreen);

                    // The drawer is left exactly as the user set it - fullscreen is about the
                    // window - so all that is left is to tell the page its height changed:
                    // some transitions do that without a window resize event.
                    window.dispatchEvent(new Event('resize'));
                });
            } catch (e) { }
          } catch (err) {
            window.__ebTitleBarError = (err && err.stack) ? err.stack : String(err);
          }
        })();
        """;
}
