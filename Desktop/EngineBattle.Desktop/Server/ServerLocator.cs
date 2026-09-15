using System.IO;
using System.Text;

namespace EngineBattle.Desktop.Server;

/// <summary>
/// Decides how to start the EngineBattle server. The shell has no compile-time reference to
/// WebGUI, so this resolution happens at runtime.
/// </summary>
internal static class ServerLocator
{
    public const string ServerExeName = "EngineBattle.exe";

    /// <summary>What was checked, in order, for the error overlay.</summary>
    private static readonly List<string> _probed = new();

    public static ServerLaunch? Locate(string[] args)
    {
        _probed.Clear();

        // 1. Explicit override: --server <path>
        for (int i = 0; i < args.Length - 1; i++)
        {
            if (args[i].Equals("--server", StringComparison.OrdinalIgnoreCase))
                return Exe(args[i + 1]);
        }

        // 2. Environment override, for scripted launches
        var fromEnv = Environment.GetEnvironmentVariable("ENGINEBATTLE_EXE");
        if (!string.IsNullOrWhiteSpace(fromEnv) && Exe(fromEnv) is { } env)
            return env;

        // 3. Released layout: both executables sit in the same folder
        if (Exe(Path.Combine(AppContext.BaseDirectory, ServerExeName)) is { } sibling)
            return sibling;

        // 4. Source checkout: run from the project so content root and wwwroot line up.
        //    Deliberately no probe of publish/ -- a stale publish folder shadowing the
        //    working tree is a trap, not a convenience.
        var webGui = FindWebGuiProjectDir(AppContext.BaseDirectory);
        if (webGui is not null)
        {
            _probed.Add($"{webGui}  (via dotnet run)");
            return ServerLaunch.DotnetRun(webGui);
        }

        return null;
    }

    private static ServerLaunch? Exe(string path)
    {
        var full = Path.GetFullPath(path.Trim('"'));
        _probed.Add(full);
        return File.Exists(full) ? ServerLaunch.PublishedExe(full) : null;
    }

    /// <summary>Walks up looking for the marker that identifies an EngineBattle checkout.</summary>
    private static string? FindWebGuiProjectDir(string start)
    {
        var dir = new DirectoryInfo(start);
        while (dir is not null)
        {
            var candidate = Path.Combine(dir.FullName, "WebGUI");
            if (File.Exists(Path.Combine(candidate, "WebGUI.csproj")))
                return candidate;
            dir = dir.Parent;
        }
        return null;
    }

    public static string DescribeProbedLocations()
    {
        var sb = new StringBuilder();
        foreach (var p in _probed) sb.AppendLine("    " + p);
        return sb.ToString();
    }
}
