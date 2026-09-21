using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text.Json;
using System.Windows;
using System.Windows.Input;
using System.Windows.Interop;
using EngineBattle.Desktop.Server;
using Microsoft.Web.WebView2.Core;

namespace EngineBattle.Desktop;

internal partial class MainWindow : Window
{
    private readonly string[] _args;
    private readonly CancellationTokenSource _shutdown = new();

    private IntPtr _hwnd;
    private ServerProcess? _server;
    private LogWindow? _logWindow;
    // The page the shell opened on: where Reload goes when the view is not on one of ours.
    private string? _startupUrl;
    private bool _closing;

    // Pre-fullscreen state, so F11 restores exactly what was there before.
    private bool _isFullScreen;
    private bool _wasMaximized;
    private (int X, int Y, int Width, int Height) _preFullScreenBounds;

    public MainWindow(string[] args)
    {
        _args = args;
        InitializeComponent();

        // Window.Icon is deliberately not set. A BitmapImage built from a multi-size .ico
        // resolves to its 16x16 frame, which Windows would then scale up for the taskbar and
        // alt-tab. Leaving it unset makes WPF use the exe's embedded icon, which keeps all
        // four sizes (16/32/48/256) so Windows can pick the right one per DPI.
        SourceInitialized += OnSourceInitialized;
        Closed += OnClosed;
        // Note for anyone adding WPF chrome here: on a scaled display this window lays out
        // wider than the screen shows it (measured: Window.ActualWidth 2560 DIP at dpiScale
        // 1.5 in a window the OS reports as 2575 physical px), so content docked or aligned
        // RIGHT is arranged past the visible edge and never appears. Dock left, or measure
        // before trusting the right-hand side.
    }

    private void OnSourceInitialized(object? sender, EventArgs e)
    {
        // The handle only exists from here on, and placement must be applied before the
        // window is first painted.
        _hwnd = new WindowInteropHelper(this).Handle;

        // The caption and border are drawn by Windows, not WPF: without this they stay light
        // whatever the app looks like.
        DarkTitleBar.Apply(_hwnd);

        // Hooked before restoring placement, because restoring can maximise straight away.
        HwndSource.FromHwnd(_hwnd)?.AddHook(WndProc);

        WindowPlacement.Restore(_hwnd);

        StateChanged += OnStateChanged;

        // The page's own F11 handler only fires while the WebView has focus. This covers the
        // case where focus sits on the WPF window instead.
        PreviewKeyDown += (_, e) =>
        {
            if (e.Key != Key.F11) return;
            ToggleFullScreen();
            e.Handled = true;
        };

        _ = StartAsync();
    }

    private IntPtr WndProc(IntPtr hwnd, int msg, IntPtr wParam, IntPtr lParam, ref bool handled)
    {
        // Only a borderless window needs the work area spelled out; a framed one is maximised
        // correctly by Windows, and these numbers would fight its own frame arithmetic.
        if (msg == BorderlessWindow.WM_GETMINMAXINFO && WindowStyle == WindowStyle.None)
            BorderlessWindow.ConstrainMaximizeToWorkArea(hwnd, lParam);

        return IntPtr.Zero;
    }

    /// <summary>Called from the single-instance listener thread when a second launch happens.</summary>
    public void BringToFront()
    {
        Dispatcher.Invoke(() =>
        {
            if (WindowState == WindowState.Minimized)
                WindowState = WindowState.Normal;

            Activate();
            Topmost = true;   // nudge past foreground-lock, then release immediately
            Topmost = false;
        });
    }

    private async Task StartAsync()
    {
        ShowLoading();
        var status = new Progress<string>(text => StatusText.Text = text);

        try
        {
            if (!IsWebView2RuntimeAvailable(out var runtimeError))
            {
                ShowError(
                    "The Microsoft Edge WebView2 runtime is required.",
                    "EngineBattle Desktop renders the EngineBattle interface with WebView2.\n\n" +
                    "Install the Evergreen WebView2 Runtime from:\n" +
                    "https://developer.microsoft.com/microsoft-edge/webview2/\n\n" +
                    runtimeError);
                return;
            }

            var launch = ServerLocator.Locate(_args);
            if (launch is null)
            {
                ShowError(
                    $"Could not find {ServerLocator.ServerExeName}.",
                    "Place it next to EngineBattleDesktop.exe, pass --server <path>, or set the " +
                    "ENGINEBATTLE_EXE environment variable. Inside a source checkout the shell " +
                    "runs WebGUI with 'dotnet run' instead.\n\nChecked:\n" +
                    ServerLocator.DescribeProbedLocations());
                return;
            }

            _server = new ServerProcess();
            await _server.StartAsync(launch, status, _shutdown.Token);

            ((IProgress<string>)status).Report("Loading interface…");

            // Keep the browser cache and profile out of the install directory, which may be
            // read-only (and is, under MSIX).
            var userDataFolder = Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                "EngineBattle", "WebView2");
            Directory.CreateDirectory(userDataFolder);

            var environment = await CoreWebView2Environment.CreateAsync(null, userDataFolder, null);
            await WebView.EnsureCoreWebView2Async(environment);

            ConfigureWebView();

            var zoom = ShellPreferences.LoadZoom();
            try { WebView.ZoomFactor = zoom; } catch { /* ignore */ }
            ShowZoomInMenu(zoom);

            // Must be registered before navigating so it runs for the first document too.
            await WebView.CoreWebView2.AddScriptToExecuteOnDocumentCreatedAsync(TitleBarScript.Script);

            // The user can pick any page as their startup page; the server resolves that
            // setting and reports the URL, so honour it instead of always opening the root.
            var startupUrl = await _server.GetStartupUrlAsync(TimeSpan.FromSeconds(5), _shutdown.Token);
            _startupUrl = startupUrl;
            WebView.CoreWebView2.Navigate(startupUrl);
            WebView.Visibility = Visibility.Visible;
            Overlay.Visibility = Visibility.Collapsed;
        }
        catch (OperationCanceledException) when (_closing)
        {
            // Window closed while starting up; nothing to report.
        }
        catch (Exception ex)
        {
            ShowError("EngineBattle could not start.", ex.Message);
        }
    }

    private void ConfigureWebView()
    {
        var core = WebView.CoreWebView2;
        var settings = core.Settings;

        // F5, Ctrl+P, Ctrl+F and friends would otherwise be swallowed by the browser instead
        // of reaching the app's own HotKeys2 bindings.
        settings.AreBrowserAcceleratorKeysEnabled = false;
        settings.AreDefaultContextMenusEnabled = false;
        settings.IsStatusBarEnabled = false;
        settings.IsSwipeNavigationEnabled = false;
        settings.IsPasswordAutosaveEnabled = false;
        settings.IsGeneralAutofillEnabled = false;
        // Left on in Release too: EngineBattle is a developer-facing tool, and without a
        // browser address bar this is the only way to inspect a layout problem that only
        // reproduces inside the shell. Opened with F12 / Ctrl+Shift+I (see TitleBarScript).
        settings.AreDevToolsEnabled = true;

        // A plain link to a website (a reference in the documentation, an engine's home page)
        // would turn the shell into a browser with no way back: no address bar, no back
        // button, and a menu talking to a page that is no longer ours. The app's own pages
        // navigate; anything else goes to the system browser, like the new-window case below.
        core.NavigationStarting += (_, e) =>
        {
            if (IsOurs(e.Uri)) return;
            e.Cancel = true;
            OpenExternally(e.Uri);
        };

        // Documentation and engine links point at real websites; those belong in a browser.
        core.NewWindowRequested += (_, e) =>
        {
            e.Handled = true;
            OpenExternally(e.Uri);
        };

        core.WebMessageReceived += OnWebMessageReceived;

        // The page learns the window state from a message, and one is only sent when the state
        // CHANGES - so a freshly loaded page would not know it was in fullscreen, and would
        // show what fullscreen hides. Every document gets the current state as it appears.
        core.DOMContentLoaded += (_, _) => PublishWindowState();

        core.ProcessFailed += (_, e) =>
        {
            if (e.ProcessFailedKind == CoreWebView2ProcessFailedKind.BrowserProcessExited)
                ShowError("The browser engine stopped unexpectedly.", e.ProcessFailedKind.ToString());
        };
    }

    private static void OpenExternally(string uri)
    {
        try
        {
            Process.Start(new ProcessStartInfo(uri) { UseShellExecute = true });
        }
        catch { /* a bad link is not worth crashing over */ }
    }

    private static bool IsWebView2RuntimeAvailable(out string error)
    {
        try
        {
            var version = CoreWebView2Environment.GetAvailableBrowserVersionString();
            error = "";
            return !string.IsNullOrEmpty(version);
        }
        catch (Exception ex)
        {
            error = ex.Message;
            return false;
        }
    }

    private void OnWebMessageReceived(object? sender, CoreWebView2WebMessageReceivedEventArgs e)
    {
        string? action;
        try
        {
            using var document = JsonDocument.Parse(e.WebMessageAsJson);
            if (document.RootElement.ValueKind != JsonValueKind.Object) return;
            if (!document.RootElement.TryGetProperty("ebWindow", out var property)) return;
            action = property.GetString();
        }
        catch (JsonException)
        {
            return;   // not one of ours
        }

        switch (action)
        {
            case "toggleFullScreen": ToggleFullScreen(); break;
            case "output": ShowOutputWindow(); break;
            case "devtools": OpenDevTools(); break;
            // Keyboard shortcuts arrive from the page: keys pressed inside the WebView never
            // reach WPF, so the menu's accelerators are handled there and posted here.
            case "zoomIn": StepZoom(+1); break;
            case "zoomOut": StepZoom(-1); break;
            case "zoomReset": SetZoom(1.0); break;
            case "reload": ReloadPage(); break;
        }
    }

    private void OnStateChanged(object? sender, EventArgs e) => PublishWindowState();

    private void PublishWindowState()
    {
        var state = _isFullScreen ? "fullscreen"
                  : WindowState == WindowState.Maximized ? "maximized"
                  : "normal";
        // The menu belongs to the frame: fullscreen is fullscreen.
        AppMenu.Visibility = _isFullScreen ? Visibility.Collapsed : Visibility.Visible;
        try
        {
            WebView.CoreWebView2?.PostWebMessageAsJson("{\"ebWindowState\":\"" + state + "\"}");
        }
        catch { /* the WebView may not be initialised yet */ }
    }

    /// <summary>
    /// True fullscreen, matching what F11 does in a browser: the window covers the whole
    /// monitor, taskbar included, giving the tournament view its full height back.
    /// </summary>
    private void ToggleFullScreen()
    {
        if (_isFullScreen) ExitFullScreen();
        else EnterFullScreen();
    }

    private void EnterFullScreen()
    {
        if (!BorderlessWindow.TryGetMonitorBounds(_hwnd, out var x, out var y, out var w, out var h))
            return;

        _wasMaximized = WindowState == WindowState.Maximized;

        // Capture the restored bounds before leaving the maximised state, otherwise the saved
        // rectangle is the maximised one and exiting fullscreen lands on the wrong size.
        if (!_wasMaximized && BorderlessWindow.TryGetWindowBounds(_hwnd, out var bx, out var by, out var bw, out var bh))
            _preFullScreenBounds = (bx, by, bw, bh);

        // Explicit bounds rather than WindowState.Maximized: maximising is clamped to the work
        // area, which is exactly what fullscreen must bypass.
        if (WindowState != WindowState.Normal) WindowState = WindowState.Normal;

        // The frame goes first: its caption and border would otherwise sit inside the monitor
        // rectangle we are about to fill, leaving the page short by their thickness. NoResize
        // as well as None: a resizable window keeps WS_THICKFRAME, whose sizing border is part
        // of the non-client area, so the page would stop a few pixels short of every edge.
        WindowStyle = WindowStyle.None;
        ResizeMode = ResizeMode.NoResize;

        _isFullScreen = true;
        BorderlessWindow.SetBounds(_hwnd, x, y, w, h);
        Activate();

        PublishWindowState();
    }

    private void ExitFullScreen()
    {
        _isFullScreen = false;
        ResizeMode = ResizeMode.CanResize;
        WindowStyle = WindowStyle.SingleBorderWindow;

        if (_wasMaximized)
        {
            WindowState = WindowState.Maximized;
        }
        else if (_preFullScreenBounds.Width > 0)
        {
            var (x, y, w, h) = _preFullScreenBounds;
            BorderlessWindow.SetBounds(_hwnd, x, y, w, h);
        }

        PublishWindowState();
    }

    // ---- menu -------------------------------------------------------------------------------

    /// Browser-like steps rather than a fixed percentage, so every press lands on a round number.
    private static readonly double[] ZoomSteps = { 0.5, 0.67, 0.75, 0.8, 0.9, 1.0, 1.1, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0 };

    private void OnMenuExit(object sender, RoutedEventArgs e) => Close();

    /// The drawer belongs to the web app, so the shell asks rather than reaches in.
    private void OnMenuToggleDrawer(object sender, RoutedEventArgs e)
    {
        try { WebView.CoreWebView2?.PostWebMessageAsJson("{\"ebCommand\":\"toggleDrawer\"}"); }
        catch { /* nothing loaded yet */ }
    }
    private void OnMenuZoomIn(object sender, RoutedEventArgs e) => StepZoom(+1);
    private void OnMenuZoomOut(object sender, RoutedEventArgs e) => StepZoom(-1);
    private void OnMenuZoomReset(object sender, RoutedEventArgs e) => SetZoom(1.0);
    private void OnMenuReload(object sender, RoutedEventArgs e) => ReloadPage();
    private void OnMenuFullScreen(object sender, RoutedEventArgs e) => ToggleFullScreen();
    private void OnMenuServerOutput(object sender, RoutedEventArgs e) => ShowOutputWindow();
    private void OnMenuDevTools(object sender, RoutedEventArgs e) => OpenDevTools();

    private void StepZoom(int direction)
    {
        var current = WebView.ZoomFactor;
        // The nearest step, so a zoom restored from disk (or set from the page) still steps sanely.
        var index = 0;
        for (var i = 1; i < ZoomSteps.Length; i++)
            if (Math.Abs(ZoomSteps[i] - current) < Math.Abs(ZoomSteps[index] - current))
                index = i;

        var next = Math.Clamp(index + direction, 0, ZoomSteps.Length - 1);
        SetZoom(ZoomSteps[next]);
    }

    private void SetZoom(double zoom)
    {
        zoom = Math.Clamp(zoom, ShellPreferences.MinZoom, ShellPreferences.MaxZoom);
        try { WebView.ZoomFactor = zoom; }
        catch { return; }   // the WebView may not be initialised yet

        ShellPreferences.SaveZoom(zoom);
        ShowZoomInMenu(zoom);
    }

    /// The Reset item carries the current level, so there is somewhere to read it off.
    private void ShowZoomInMenu(double zoom) =>
        ResetZoomItem.Header = zoom == 1.0 ? "_Reset Zoom" : $"_Reset Zoom (now {zoom * 100:0}%)";

    private void ReloadPage()
    {
        // Blazor Server: this drops the circuit and builds a new one, so the page starts over.
        // Tournaments and analysis run in the server's own singletons and keep going.
        // Off one of our pages (it should not happen any more, but a stuck view must have a
        // way home), Reload is the way back to the start page.
        try
        {
            var core = WebView.CoreWebView2;
            if (core is null) return;
            if (!IsOurs(core.Source) && !string.IsNullOrEmpty(_startupUrl)) core.Navigate(_startupUrl);
            else core.Reload();
        }
        catch { /* nothing loaded yet */ }
    }

    /// True for the app's own pages - the server's host and port - and for anything that is
    /// not a website at all (about:blank, the startup navigation before the server has said
    /// where it lives). Only http(s) elsewhere is "somewhere else".
    private bool IsOurs(string? uri)
    {
        var baseUrl = _server?.BaseUrl;
        if (string.IsNullOrEmpty(uri) || string.IsNullOrEmpty(baseUrl)) return true;
        if (!Uri.TryCreate(uri, UriKind.Absolute, out var u) || !Uri.TryCreate(baseUrl, UriKind.Absolute, out var b)) return true;
        if (u.Scheme != Uri.UriSchemeHttp && u.Scheme != Uri.UriSchemeHttps) return true;
        var sameHost = string.Equals(u.Host, b.Host, StringComparison.OrdinalIgnoreCase) || (u.IsLoopback && b.IsLoopback);
        return sameHost && u.Port == b.Port;
    }

    private void OnMenuAbout(object sender, RoutedEventArgs e)
    {
        var shell = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version?.ToString() ?? "unknown";
        var runtime = "not found";
        try { runtime = CoreWebView2Environment.GetAvailableBrowserVersionString(); }
        catch { /* leave the default */ }

        MessageBox.Show(this,
            $"EngineBattle desktop shell {shell}\n" +
            $"WebView2 runtime {runtime}\n" +
            $"Server {(string.IsNullOrEmpty(_server?.BaseUrl) ? "not started" : _server!.BaseUrl)}\n\n" +
            "The shell hosts EngineBattle's web interface; the chess engine work happens in the server process.",
            "About EngineBattle", MessageBoxButton.OK, MessageBoxImage.Information);
    }

    private void OpenDevTools()
    {
        try { WebView.CoreWebView2?.OpenDevToolsWindow(); }
        catch { /* devtools unavailable */ }
    }

    private void ShowOutputWindow()
    {
        if (_server is null) return;

        if (_logWindow is null)
        {
            _logWindow = new LogWindow(_server) { Owner = this };
            _logWindow.Closed += (_, _) => _logWindow = null;
            _logWindow.Show();
        }
        else
        {
            if (_logWindow.WindowState == WindowState.Minimized)
                _logWindow.WindowState = WindowState.Normal;
            _logWindow.Activate();
        }
    }


    private void ShowLoading()
    {
        Overlay.Visibility = Visibility.Visible;
        LoadingPanel.Visibility = Visibility.Visible;
        ErrorPanel.Visibility = Visibility.Collapsed;
    }

    private void ShowError(string title, string detail)
    {
        Overlay.Visibility = Visibility.Visible;
        LoadingPanel.Visibility = Visibility.Collapsed;
        ErrorPanel.Visibility = Visibility.Visible;
        ErrorTitle.Text = title;
        ErrorDetail.Text = detail;
    }

    private async void OnRetryClick(object sender, RoutedEventArgs e)
    {
        _server?.Dispose();
        _server = null;
        await StartAsync();
    }

    private void OnShowOutputClick(object sender, RoutedEventArgs e) => ShowOutputWindow();

    private void OnCloseClick(object sender, RoutedEventArgs e) => Close();

    private void OnCopyClick(object sender, RoutedEventArgs e)
    {
        try
        {
            Clipboard.SetText($"{ErrorTitle.Text}{Environment.NewLine}{Environment.NewLine}{ErrorDetail.Text}");
        }
        catch { /* clipboard can be locked by another process */ }
    }

    private void OnClosed(object? sender, EventArgs e)
    {
        _closing = true;
        WindowPlacement.Save(_hwnd);
        _shutdown.Cancel();
        _server?.Dispose();
        _shutdown.Dispose();

        // Belt and braces alongside ShutdownMode="OnMainWindowClose". A process that survives
        // its own window keeps the single-instance mutex and makes later launches look dead.
        Application.Current?.Shutdown();
    }
}
