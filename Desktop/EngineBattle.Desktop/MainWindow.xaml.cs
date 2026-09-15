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
    }

    private void OnSourceInitialized(object? sender, EventArgs e)
    {
        // The handle only exists from here on, and placement must be applied before the
        // window is first painted.
        _hwnd = new WindowInteropHelper(this).Handle;

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

        ApplyResizeGutter();

        _ = StartAsync();
    }

    private IntPtr WndProc(IntPtr hwnd, int msg, IntPtr wParam, IntPtr lParam, ref bool handled)
    {
        if (msg == BorderlessWindow.WM_GETMINMAXINFO)
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

            // Must be registered before navigating so it runs for the first document too.
            await WebView.CoreWebView2.AddScriptToExecuteOnDocumentCreatedAsync(TitleBarScript.Script);

            // The user can pick any page as their startup page; the server resolves that
            // setting and reports the URL, so honour it instead of always opening the root.
            var startupUrl = await _server.GetStartupUrlAsync(TimeSpan.FromSeconds(5), _shutdown.Token);
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

        // Documentation and engine links point at real websites; those belong in a browser.
        core.NewWindowRequested += (_, e) =>
        {
            e.Handled = true;
            OpenExternally(e.Uri);
        };

        core.WebMessageReceived += OnWebMessageReceived;

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
            case "drag": BeginNativeDrag(); break;
            case "minimize": WindowState = WindowState.Minimized; break;
            case "toggleMaximize": ToggleMaximize(); break;
            case "toggleFullScreen": ToggleFullScreen(); break;
            case "close": Close(); break;
            case "output": ShowOutputWindow(); break;
            case "devtools": OpenDevTools(); break;
        }
    }

    /// <summary>
    /// Hands the drag to Windows so snapping, monitor changes and restore-on-drag all behave
    /// natively, instead of moving the window by hand from mouse deltas.
    /// </summary>
    private void BeginNativeDrag()
    {
        ReleaseCapture();
        SendMessage(_hwnd, WM_NCLBUTTONDOWN, (IntPtr)HTCAPTION, IntPtr.Zero);
    }

    private void ToggleMaximize() =>
        WindowState = WindowState == WindowState.Maximized ? WindowState.Normal : WindowState.Maximized;

    private void OnStateChanged(object? sender, EventArgs e) => PublishWindowState();

    private void PublishWindowState()
    {
        ApplyResizeGutter();

        var state = _isFullScreen ? "fullscreen"
                  : WindowState == WindowState.Maximized ? "maximized"
                  : "normal";
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
        // area by the WM_GETMINMAXINFO hook, which is exactly what fullscreen must bypass.
        if (WindowState != WindowState.Normal) WindowState = WindowState.Normal;

        _isFullScreen = true;
        BorderlessWindow.SetBounds(_hwnd, x, y, w, h);
        Activate();

        PublishWindowState();
    }

    private void ExitFullScreen()
    {
        _isFullScreen = false;

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

    /// <summary>
    /// A maximised window has no edges to resize, and the gutter would otherwise show as a
    /// border strip against the screen edge, so it only applies while the window is restored.
    /// </summary>
    private void ApplyResizeGutter() =>
        RootHost.Margin = _isFullScreen || WindowState == WindowState.Maximized
            ? new Thickness(0)
            : new Thickness(ResizeGutter);

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

    private const int ResizeGutter = 6;
    private const int WM_NCLBUTTONDOWN = 0x00A1;
    private const int HTCAPTION = 2;

    [DllImport("user32.dll")]
    private static extern bool ReleaseCapture();

    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    private static extern IntPtr SendMessage(IntPtr hWnd, int msg, IntPtr wParam, IntPtr lParam);

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
