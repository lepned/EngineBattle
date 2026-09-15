using System.IO;
using System.Runtime.InteropServices;
using System.Text.Json;

namespace EngineBattle.Desktop;

/// <summary>
/// Remembers window size, position and maximized state between runs.
///
/// Everything here works in physical pixels through Win32 GetWindowPlacement /
/// SetWindowPlacement. That API reports the *restore* bounds even while the window is
/// maximized, and clamps to a currently attached monitor on the way back in. Reading the
/// live window size while maximized would instead persist the screen size as the restore
/// size, so un-maximizing after a restart would do nothing.
///
/// Stored under LocalAppData, never beside the executable, so it also works from a
/// read-only install directory.
/// </summary>
internal static class WindowPlacement
{
    private sealed record Saved(int Left, int Top, int Right, int Bottom, bool Maximized);

    private static string FilePath => Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
        "EngineBattle", "desktop-window.json");

    public static void Restore(IntPtr hwnd)
    {
        var saved = Read() ?? FirstRunDefault(hwnd);
        if (saved is null) return;

        var placement = new WINDOWPLACEMENT
        {
            length = (uint)Marshal.SizeOf<WINDOWPLACEMENT>(),
            showCmd = saved.Maximized ? SW_SHOWMAXIMIZED : SW_SHOWNORMAL,
            rcNormalPosition = new RECT
            {
                left = saved.Left,
                top = saved.Top,
                right = saved.Right,
                bottom = saved.Bottom,
            },
        };

        SetWindowPlacement(hwnd, ref placement);
    }

    /// <summary>
    /// A generous fraction of the monitor work area, so the default suits any display and any
    /// DPI scaling. A fixed pixel size would look tiny on a 4K panel.
    /// </summary>
    private static Saved? FirstRunDefault(IntPtr hwnd)
    {
        var monitor = MonitorFromWindow(hwnd, MONITOR_DEFAULTTOPRIMARY);
        var info = new MONITORINFO { cbSize = (uint)Marshal.SizeOf<MONITORINFO>() };
        if (monitor == IntPtr.Zero || !GetMonitorInfo(monitor, ref info)) return null;

        var work = info.rcWork;
        int workWidth = work.right - work.left;
        int workHeight = work.bottom - work.top;

        int width = Math.Min(workWidth, Math.Max(1200, (int)(workWidth * 0.78)));
        int height = Math.Min(workHeight, Math.Max(800, (int)(workHeight * 0.82)));

        int left = work.left + (workWidth - width) / 2;
        int top = work.top + (workHeight - height) / 2;

        return new Saved(left, top, left + width, top + height, Maximized: false);
    }

    public static void Save(IntPtr hwnd)
    {
        try
        {
            if (hwnd == IntPtr.Zero) return;

            var placement = new WINDOWPLACEMENT { length = (uint)Marshal.SizeOf<WINDOWPLACEMENT>() };
            if (!GetWindowPlacement(hwnd, ref placement)) return;

            var r = placement.rcNormalPosition;
            if (r.right - r.left < 200 || r.bottom - r.top < 200) return;

            // A minimized window should come back as a normal one, not minimized.
            var saved = new Saved(r.left, r.top, r.right, r.bottom, placement.showCmd == SW_SHOWMAXIMIZED);

            Directory.CreateDirectory(Path.GetDirectoryName(FilePath)!);
            File.WriteAllText(FilePath, JsonSerializer.Serialize(saved));
        }
        catch { /* window placement is never worth failing a shutdown over */ }
    }

    private static Saved? Read()
    {
        try
        {
            if (!File.Exists(FilePath)) return null;
            var s = JsonSerializer.Deserialize<Saved>(File.ReadAllText(FilePath));
            if (s is null) return null;
            return s.Right - s.Left >= 200 && s.Bottom - s.Top >= 200 ? s : null;
        }
        catch { return null; }
    }

    private const uint SW_SHOWNORMAL = 1;
    private const uint SW_SHOWMAXIMIZED = 3;
    private const uint MONITOR_DEFAULTTOPRIMARY = 1;

    [StructLayout(LayoutKind.Sequential)]
    private struct POINT { public int x, y; }

    [StructLayout(LayoutKind.Sequential)]
    private struct RECT { public int left, top, right, bottom; }

    [StructLayout(LayoutKind.Sequential)]
    private struct WINDOWPLACEMENT
    {
        public uint length;
        public uint flags;
        public uint showCmd;
        public POINT ptMinPosition;
        public POINT ptMaxPosition;
        public RECT rcNormalPosition;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct MONITORINFO
    {
        public uint cbSize;
        public RECT rcMonitor;
        public RECT rcWork;
        public uint dwFlags;
    }

    [DllImport("user32.dll", SetLastError = true)]
    private static extern bool GetWindowPlacement(IntPtr hWnd, ref WINDOWPLACEMENT lpwndpl);

    [DllImport("user32.dll", SetLastError = true)]
    private static extern bool SetWindowPlacement(IntPtr hWnd, ref WINDOWPLACEMENT lpwndpl);

    [DllImport("user32.dll")]
    private static extern IntPtr MonitorFromWindow(IntPtr hwnd, uint dwFlags);

    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    private static extern bool GetMonitorInfo(IntPtr hMonitor, ref MONITORINFO lpmi);
}
