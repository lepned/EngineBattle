using System.IO;

namespace EngineBattle.Desktop.Server;

/// <summary>
/// How to start the EngineBattle server. A released build launches the published exe
/// directly; a source checkout goes through <c>dotnet run</c>, which is the only dev form
/// that works — the plain build output has no <c>wwwroot</c> beside it and cannot self-host.
/// </summary>
internal sealed record ServerLaunch(
    string FileName,
    IReadOnlyList<string> LeadingArgs,
    string WorkingDirectory,
    string Description,
    bool NeedsArgumentSeparator)
{
    public static ServerLaunch PublishedExe(string exePath) => new(
        FileName: exePath,
        LeadingArgs: Array.Empty<string>(),
        // The server resolves tournament, engine and log paths against the current directory,
        // so start it in its own folder to match double-click behaviour exactly.
        WorkingDirectory: Path.GetDirectoryName(exePath)!,
        Description: exePath,
        NeedsArgumentSeparator: false);

    public static ServerLaunch DotnetRun(string webGuiProjectDir)
    {
#if DEBUG
        const string configuration = "Debug";
#else
        const string configuration = "Release";
#endif
        return new ServerLaunch(
            FileName: "dotnet",
            LeadingArgs: new[] { "run", "--project", webGuiProjectDir, "-c", configuration },
            WorkingDirectory: webGuiProjectDir,
            Description: $"dotnet run --project {webGuiProjectDir} -c {configuration}",
            NeedsArgumentSeparator: true);
    }
}
