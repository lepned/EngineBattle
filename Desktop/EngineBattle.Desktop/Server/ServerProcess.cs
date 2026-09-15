using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;

namespace EngineBattle.Desktop.Server;

/// <summary>
/// Owns the EngineBattle.exe child process: picks a free port, launches it with the browser
/// auto-launch suppressed, waits until it serves HTTP, and relays everything it prints.
/// </summary>
internal sealed class ServerProcess : IDisposable
{
    private const int DefaultPort = 5018;
    private const int RetainedLines = 5000;
    private const int ErrorPreviewLines = 40;
    private static readonly TimeSpan ReadyTimeout = TimeSpan.FromSeconds(120);

    /// <summary>
    /// The server prints this once it has resolved the user's configured startup page. Parsing
    /// stdout keeps that setting owned by the server rather than duplicating settings-file
    /// parsing in the shell.
    /// </summary>
    private const string StartupUrlMarker = "EngineBattle startup URL:";

    private readonly TaskCompletionSource<string> _startupUrl =
        new(TaskCreationOptions.RunContinuationsAsynchronously);

    private readonly JobObject _job = new();
    private readonly Queue<string> _lines = new();
    private readonly object _outputLock = new();
    private Process? _process;
    private StreamWriter? _log;
    private IProgress<string>? _status;
    private bool _reportedBuilding;
    private bool _reportedBuilt;

    public string BaseUrl { get; private set; } = "";

    /// <summary>Raised for every line the server prints, from a background thread.</summary>
    public event Action<string>? LineWritten;

    /// <summary>
    /// Where the server's stdout/stderr is mirrored. A WinExe has no console, so without this
    /// everything EngineBattle prints would be lost. Truncated per run to stay small; the
    /// server's own Serilog file under its working directory is the durable log.
    /// </summary>
    public static string LogPath => Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
        "EngineBattle", "desktop-server.log");

    /// <summary>Everything captured so far, for a window that opens mid-run.</summary>
    public string[] Snapshot()
    {
        lock (_outputLock) return _lines.ToArray();
    }

    /// <summary>The tail, for the startup-failure overlay.</summary>
    public string RecentOutput
    {
        get
        {
            lock (_outputLock)
                return string.Join(Environment.NewLine, _lines.TakeLast(ErrorPreviewLines));
        }
    }

    /// <summary>
    /// The page to open: the user's configured startup page when the server reports one,
    /// otherwise the site root. Never blocks for long — the value normally arrives before the
    /// server starts answering HTTP at all.
    /// </summary>
    public async Task<string> GetStartupUrlAsync(TimeSpan timeout, CancellationToken ct)
    {
        var completed = await Task.WhenAny(_startupUrl.Task, Task.Delay(timeout, ct));
        return completed == _startupUrl.Task ? _startupUrl.Task.Result : BaseUrl;
    }

    public async Task StartAsync(ServerLaunch launch, IProgress<string> status, CancellationToken ct)
    {
        int port = FindFreePort(DefaultPort);
        BaseUrl = $"http://localhost:{port}";

        var psi = new ProcessStartInfo(launch.FileName)
        {
            WorkingDirectory = launch.WorkingDirectory,
            UseShellExecute = false,
            CreateNoWindow = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
        };

        foreach (var arg in launch.LeadingArgs) psi.ArgumentList.Add(arg);
        if (launch.NeedsArgumentSeparator) psi.ArgumentList.Add("--");
        psi.ArgumentList.Add("--urls");
        psi.ArgumentList.Add(BaseUrl);
        psi.ArgumentList.Add("--no-browser");

        _status = status;
        status.Report("Starting EngineBattle…");

        OpenLog(launch);

        _process = Process.Start(psi)
            ?? throw new InvalidOperationException($"Could not start {launch.Description}.");

        // Drain both pipes; a full pipe would block the server.
        _process.OutputDataReceived += (_, e) => Capture(e.Data);
        _process.ErrorDataReceived += (_, e) => Capture(e.Data);
        _process.BeginOutputReadLine();
        _process.BeginErrorReadLine();

        _job.Assign(_process.Handle);

        await WaitForReadyAsync(status, ct);
    }

    private void OpenLog(ServerLaunch launch)
    {
        try
        {
            Directory.CreateDirectory(Path.GetDirectoryName(LogPath)!);
            _log = new StreamWriter(LogPath, append: false) { AutoFlush = true };
            _log.WriteLine($"=== EngineBattle Desktop {DateTime.Now:yyyy-MM-dd HH:mm:ss} ===");
            _log.WriteLine($"launch : {launch.Description}");
            _log.WriteLine($"workdir: {launch.WorkingDirectory}");
            _log.WriteLine($"url    : {BaseUrl}");
            _log.WriteLine();
        }
        catch
        {
            // A missing log must never stop the app from starting.
            _log = null;
        }
    }

    private void Capture(string? line)
    {
        if (line is null) return;

        lock (_outputLock)
        {
            _lines.Enqueue(line);
            while (_lines.Count > RetainedLines) _lines.Dequeue();

            try { _log?.WriteLine(line); } catch { /* disk full, locked, ... */ }
        }

        ReportBuildProgress(line);

        var marker = line.IndexOf(StartupUrlMarker, StringComparison.Ordinal);
        if (marker >= 0)
        {
            var url = line[(marker + StartupUrlMarker.Length)..].Trim();
            if (url.StartsWith("http", StringComparison.OrdinalIgnoreCase))
                _startupUrl.TrySetResult(url);
        }

        // Raised outside the lock: a subscriber marshalling to the UI thread must never be
        // able to block the pipe readers.
        LineWritten?.Invoke(line);
    }

    /// <summary>
    /// In a source checkout the server starts through `dotnet run`, which compiles first if
    /// anything changed. Without this the splash reads "Waiting for the server..." for the whole
    /// build, making a slow rebuild indistinguishable from a hang.
    /// </summary>
    private void ReportBuildProgress(string line)
    {
        if (_status is null || _reportedBuilt) return;

        // `dotnet run` announces a compile with exactly this line before it blocks.
        if (!_reportedBuilding &&
            (line.Trim().Equals("Building...", StringComparison.Ordinal) ||
             line.Contains("Determining projects to restore", StringComparison.Ordinal)))
        {
            _reportedBuilding = true;
            _status.Report("Building WebGUI… (first run after a source change)");
            return;
        }

        // The server's own first line of output: the build is over and it is starting.
        if (_reportedBuilding && line.Contains("Runtime version:", StringComparison.Ordinal))
        {
            _reportedBuilt = true;
            _status.Report("Starting EngineBattle…");
        }
    }

    private async Task WaitForReadyAsync(IProgress<string> status, CancellationToken ct)
    {
        status.Report("Waiting for the server…");

        using var http = new HttpClient { Timeout = TimeSpan.FromSeconds(5) };
        var deadline = DateTime.UtcNow + ReadyTimeout;

        while (DateTime.UtcNow < deadline)
        {
            ct.ThrowIfCancellationRequested();

            if (_process is { HasExited: true })
            {
                throw new InvalidOperationException(
                    $"EngineBattle exited during startup (exit code {_process.ExitCode})." +
                    Environment.NewLine + Environment.NewLine + RecentOutput);
            }

            try
            {
                using var response = await http.GetAsync(BaseUrl, HttpCompletionOption.ResponseHeadersRead, ct);
                if (response.IsSuccessStatusCode) return;
            }
            catch (OperationCanceledException) when (ct.IsCancellationRequested)
            {
                throw;
            }
            catch
            {
                // Not listening yet.
            }

            await Task.Delay(200, ct);
        }

        throw new TimeoutException(
            $"EngineBattle did not respond on {BaseUrl} within {ReadyTimeout.TotalSeconds:F0} seconds." +
            Environment.NewLine + Environment.NewLine + RecentOutput);
    }

    /// <summary>Asks the OS for an unused loopback port, starting from the server's own default.</summary>
    private static int FindFreePort(int startPort)
    {
        for (int port = startPort; port <= startPort + 100; port++)
        {
            try
            {
                using var listener = new TcpListener(IPAddress.Loopback, port);
                listener.Start();
                listener.Stop();
                return port;
            }
            catch (SocketException) { }
        }

        // Fall back to whatever the OS hands out.
        using var any = new TcpListener(IPAddress.Loopback, 0);
        any.Start();
        int assigned = ((IPEndPoint)any.LocalEndpoint).Port;
        any.Stop();
        return assigned;
    }

    public void Dispose()
    {
        try
        {
            if (_process is { HasExited: false })
                _process.Kill(entireProcessTree: true);
        }
        catch { /* already gone */ }

        _process?.Dispose();
        _job.Dispose();   // backstop: kills the child even if Kill above failed

        lock (_outputLock)
        {
            try { _log?.Dispose(); } catch { }
            _log = null;
        }
    }
}
