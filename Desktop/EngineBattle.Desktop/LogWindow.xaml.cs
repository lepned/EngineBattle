using System.Diagnostics;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Threading;
using EngineBattle.Desktop.Server;

namespace EngineBattle.Desktop;

/// <summary>
/// Live view of the server's stdout/stderr. A WinExe has no console window, so this is the
/// only place that output is visible while the app runs.
/// </summary>
internal partial class LogWindow : Window
{
    private const int MaxCharacters = 2_000_000;

    private readonly ServerProcess _server;
    private readonly Queue<string> _pending = new();
    private readonly object _pendingLock = new();
    private readonly DispatcherTimer _flushTimer;

    public LogWindow(ServerProcess server)
    {
        _server = server;
        InitializeComponent();

        Output.Text = string.Join(Environment.NewLine, server.Snapshot());
        Output.ScrollToEnd();

        // Batched rather than per-line: a chatty engine would otherwise post thousands of
        // dispatcher callbacks a second.
        _flushTimer = new DispatcherTimer(DispatcherPriority.Background)
        {
            Interval = TimeSpan.FromMilliseconds(150),
        };
        _flushTimer.Tick += (_, _) => Flush();
        _flushTimer.Start();

        _server.LineWritten += OnLineWritten;
        Closed += (_, _) =>
        {
            _server.LineWritten -= OnLineWritten;
            _flushTimer.Stop();
        };
    }

    private void OnLineWritten(string line)
    {
        lock (_pendingLock) _pending.Enqueue(line);
    }

    private void Flush()
    {
        string[] batch;
        lock (_pendingLock)
        {
            if (_pending.Count == 0) return;
            batch = _pending.ToArray();
            _pending.Clear();
        }

        var sb = new StringBuilder();
        foreach (var line in batch) sb.AppendLine(line);

        if (Output.Text.Length > MaxCharacters)
            Output.Text = Output.Text[(Output.Text.Length / 2)..];

        Output.AppendText(sb.ToString());

        if (AutoScroll.IsChecked == true) Output.ScrollToEnd();
    }

    private void OnCopyAll(object sender, RoutedEventArgs e)
    {
        try { Clipboard.SetText(Output.Text); }
        catch { /* clipboard can be locked by another process */ }
    }

    private void OnOpenLogFile(object sender, RoutedEventArgs e)
    {
        try
        {
            if (File.Exists(ServerProcess.LogPath))
                Process.Start(new ProcessStartInfo(ServerProcess.LogPath) { UseShellExecute = true });
        }
        catch { /* no handler registered for .log */ }
    }

    private void OnClear(object sender, RoutedEventArgs e) => Output.Clear();
}
