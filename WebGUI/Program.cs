
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using MudBlazor.Services;
using Serilog;
using Serilog.Events;
using System.Runtime.InteropServices;
using Toolbelt.Blazor.Extensions.DependencyInjection;
using WebGUI.Components;
using WebGUI.Services;

static int FindAvailablePort(int startPort)
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
    return 0;
}

/// <summary>
/// Finds the CSS isolation bundle directory in obj/ for the active build configuration.
/// Returns empty string if not found (e.g. in published builds where the bundle is already in wwwroot).
/// </summary>
static string FindCssBundleDir(IWebHostEnvironment env)
{
    var cssName = System.Reflection.Assembly.GetExecutingAssembly().GetName().Name + ".styles.css";

    // Published builds already have the CSS in wwwroot — no fallback needed
    if (File.Exists(Path.Combine(env.WebRootPath, cssName))) return "";

    // In dev mode, search obj/ for the bundle directory matching the build configuration
    var contentRoot = env.ContentRootPath;
#if DEBUG
    var configDir = Path.Combine(contentRoot, "obj", "Debug");
#else
    var configDir = Path.Combine(contentRoot, "obj", "Release");
#endif
    if (!Directory.Exists(configDir))
        configDir = Path.Combine(contentRoot, "obj");
    if (!Directory.Exists(configDir)) return "";

    try
    {
        var source = Directory.GetFiles(configDir, cssName, SearchOption.AllDirectories).FirstOrDefault();
        return source != null ? Path.GetDirectoryName(source) : "";
    }
    catch { return ""; }
}

static void EnsureTournamentJsonIsLoaded()
{
    string currentDir = AppPaths.BaseDir;

    string path = Path.Combine(currentDir, "Data", "tournamentEmpty.json");
    string dest = Path.Combine(currentDir, "wwwroot", "tournament.json");

    if (!File.Exists(dest))
    {
        if (!File.Exists(path))
        {
            Console.WriteLine($"Note: source template not found at {path}, skipping tournament.json copy.");
            return;
        }
        Console.WriteLine("***Note: tournament.json file not found, copying a default version from Data folder.");
        Console.WriteLine($"Copying file to {dest}\n");
        Directory.CreateDirectory(Path.GetDirectoryName(dest)!);
        File.Copy(path, dest, true);
    }
}

var logPath = Path.Combine(Environment.CurrentDirectory, "logs", "log-{Date}.txt");

var log = new LoggerConfiguration()
    .MinimumLevel.Debug()
    .MinimumLevel.Override("Microsoft", LogEventLevel.Warning)
    .MinimumLevel.Override("Microsoft.AspNetCore.Components", LogEventLevel.Warning)
    .WriteTo.File(
        path: logPath,
        rollingInterval: RollingInterval.Day,
        rollOnFileSizeLimit: true,
        fileSizeLimitBytes: 10_000_000)
        .CreateLogger();

var builder = WebApplication.CreateBuilder(args);
// Host shutdown timeout
builder.Services.Configure<HostOptions>(o => o.ShutdownTimeout = TimeSpan.FromSeconds(3));

// Process command-line arguments
if (args.Length > 0)
{
    foreach (var arg in args)
    {
        Console.WriteLine($"Argument: {arg}");
    }

    // Example: Check for a specific argument
    if (args.Contains("--enable-logging"))
    {
        Console.WriteLine("Logging is enabled via command-line argument.");
        // Perform any specific configuration based on the argument
    }
}

builder.Services.AddRazorComponents()
    .AddInteractiveServerComponents();

// Add Circuit Options to allow better handling of multiple clients
builder.Services.AddServerSideBlazor()
    .AddCircuitOptions(options =>
    {
      options.DisconnectedCircuitMaxRetained = 100; // Max disconnected circuits
      options.DisconnectedCircuitRetentionPeriod = TimeSpan.FromMinutes(3); // Retention time
    });

// Configure SignalR to handle multiple connections
builder.Services.AddSignalR(options =>
{
  options.MaximumReceiveMessageSize = 32 * 1024; // Allow large messages
  options.EnableDetailedErrors = true;          // Enable detailed error messages
});

builder.Logging.AddSerilog(log);

// Add services to the container.
//builder.Services.AddRazorPages();

builder.Services.AddMudServices();
builder.Services.AddHotKeys2();
builder.Services.AddScoped<NotifierService>();
builder.Services.AddScoped<ChessConfigurationService>();
builder.Services.AddScoped<JavaScriptInteropService>();
builder.Services.AddScoped<ClipboardService>();
builder.Services.AddScoped<GameReviewService>();
builder.Services.AddScoped<AnalysisNavigationService>();
builder.Services.AddSingleton<OverlaySetting>();
builder.Services.AddSingleton<OpeningExplorerService>();
// Register the shutdown token provider so UI/services can link the tournament
builder.Services.AddSingleton<ShutdownTokenProvider>();
builder.Services.AddSingleton<TournamentService>();
builder.Services.AddSingleton<JsonFeedService>();
builder.Services.AddSingleton<LiveFeedReplayer>();
builder.Services.AddSingleton<GlobalSettingsService>();

Console.OutputEncoding = System.Text.Encoding.UTF8;

var app = builder.Build();

// Use ContentRootPath — correct in both dev (project dir) and published (exe dir) modes
AppPaths.BaseDir = app.Environment.ContentRootPath;

// Get the .NET runtime version
Console.WriteLine("Runtime version: " + Environment.Version);

// Get the full framework description
Console.WriteLine("Framework: " + RuntimeInformation.FrameworkDescription);

app.MapStaticAssets();

// CSS isolation fallback: serve CSS bundle directly from obj/ if not in wwwroot (dev mode with stale builds)
var cssBundleDir = FindCssBundleDir(app.Environment);
if (!string.IsNullOrEmpty(cssBundleDir))
{
    app.UseStaticFiles(new StaticFileOptions
    {
        FileProvider = new Microsoft.Extensions.FileProviders.PhysicalFileProvider(cssBundleDir),
        ContentTypeProvider = new Microsoft.AspNetCore.StaticFiles.FileExtensionContentTypeProvider(
            new Dictionary<string, string> { [".css"] = "text/css" })
    });
}
app.UseAntiforgery();
app.MapRazorComponents<App>()
    .AddInteractiveServerRenderMode()
    .WithStaticAssets();

// Live-feed ingest endpoint: external runners POST NDJSON wire events here (one per line, or a
// batch). The source (server) is taken from the X-Feed-Source header or the remote IP and stamped
// onto each event so multiple servers' games stay distinct in one grid. See LiveFeedContract.md.
app.MapPost("/api/livefeed", async (HttpContext ctx, JsonFeedService feed) =>
{
    var token = Environment.GetEnvironmentVariable("EB_LIVEFEED_TOKEN");
    if (!string.IsNullOrEmpty(token) && ctx.Request.Headers["X-Feed-Token"] != token)
        return Results.Unauthorized();

    var source = ctx.Request.Headers["X-Feed-Source"].ToString();
    if (string.IsNullOrEmpty(source))
        source = ctx.Connection.RemoteIpAddress?.ToString() ?? "";

    int n = 0;
    using var reader = new StreamReader(ctx.Request.Body);
    for (var line = await reader.ReadLineAsync(); line != null; line = await reader.ReadLineAsync())
    {
        if (string.IsNullOrWhiteSpace(line)) continue;
        var stamped = string.IsNullOrEmpty(source) ? line : ChessLibrary.LiveFeedWire.withSource(source, line);
        if (feed.Ingest(stamped)) n++;
    }
    return Results.Ok(new { ingested = n });
});

EnsureTournamentJsonIsLoaded();

bool hasExplicitUrls = args.Any(a => a.StartsWith("--urls", StringComparison.OrdinalIgnoreCase))
    || Environment.GetEnvironmentVariable("ASPNETCORE_URLS") != null;

if (!hasExplicitUrls && !app.Environment.IsDevelopment())
{
    const int defaultPort = 5018;
    int port = FindAvailablePort(defaultPort);
    if (port > 0)
    {
        app.Urls.Clear();
        app.Urls.Add($"http://localhost:{port}");
        if (port != defaultPort)
            Console.WriteLine($"Port {defaultPort} in use, using port {port} instead.");
    }
}

app.Lifetime.ApplicationStarted.Register(() =>
{
    var startupPage = app.Services.GetRequiredService<GlobalSettingsService>().Settings.StartupPage;
    var page = string.IsNullOrEmpty(startupPage) ? "/" : startupPage;
    var address = (app.Urls.FirstOrDefault() ?? "http://localhost:5000").TrimEnd('/') + page;
    try
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            Process.Start(new ProcessStartInfo(address) { UseShellExecute = true });
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            Process.Start("open", address);
        else
            Process.Start("xdg-open", address);
    }
    catch { }
});

app.Run();

public static class AppPaths
{
    public static string BaseDir { get; set; } = AppContext.BaseDirectory;
}
