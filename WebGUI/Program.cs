
using Toolbelt.Blazor.Extensions.DependencyInjection;
using MudBlazor.Services;
using WebGUI.Services;
using Serilog;
using Serilog.Events;
using WebGUI.Components;
using System.Runtime.InteropServices;
using static ChessLibrary.Utilities;

static void EnsureTournamentJsonIsLoaded()
{
    string currentDir = Environment.CurrentDirectory;    

    string path = Path.Combine(currentDir, "Data", "tournamentEmpty.json");
    string dest = Path.Combine(currentDir, "wwwroot", "tournament.json");    

    if (!File.Exists(dest))   
    {
        Console.WriteLine("***Note: tournament.json file not found, copying a default version from Data folder.");
        Console.WriteLine($"Copying file to {dest}\n");
        File.Copy(path, dest, true);
    }
}

var log = new LoggerConfiguration()
    .MinimumLevel.Debug()
    .MinimumLevel.Override("Microsoft", LogEventLevel.Warning)
    .MinimumLevel.Override("Microsoft.AspNetCore.Components", LogEventLevel.Warning)
    .WriteTo.File(
        path: "..\\logs\\log-{Date}.txt",
        rollingInterval: RollingInterval.Day,
        rollOnFileSizeLimit: true,
        fileSizeLimitBytes: 10_000_000)
    .CreateLogger();

var builder = WebApplication.CreateBuilder(args);
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
builder.Services.AddSingleton<OverlaySetting>();
Console.OutputEncoding = System.Text.Encoding.UTF8;

var app = builder.Build();

// Get the .NET runtime version
Console.WriteLine("Runtime version: " + Environment.Version);

// Get the full framework description
Console.WriteLine("Framework: " + RuntimeInformation.FrameworkDescription);

app.UseStaticFiles();
app.UseAntiforgery();
app.MapRazorComponents<App>()
    .AddInteractiveServerRenderMode();
EnsureTournamentJsonIsLoaded();
app.Run();
