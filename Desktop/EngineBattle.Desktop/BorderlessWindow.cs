using System.Runtime.InteropServices;

namespace EngineBattle.Desktop;

/// <summary>
/// Makes a WindowStyle=None window maximise like a normal one.
///
/// A window with a system title bar maximises to the monitor's *work area*, which excludes the
/// taskbar. A borderless window does not: Windows sizes it to the full monitor rectangle, so
/// the bottom of the page ends up behind the taskbar — and, worse, still inside the viewport,
/// so page code measuring window.innerHeight believes that hidden strip is usable.
/// </summary>
internal static class BorderlessWindow
{
    public const int WM_GETMINMAXINFO = 0x0024;

    public static void ConstrainMaximizeToWorkArea(IntPtr hwnd, IntPtr lParam)
    {
        var monitor = MonitorFromWindow(hwnd, MONITOR_DEFAULTTONEAREST);
        if (monitor == IntPtr.Zero) return;

        var info = new MONITORINFO { cbSize = (uint)Marshal.SizeOf<MONITORINFO>() };
        if (!GetMonitorInfo(monitor, ref info)) return;

        var mmi = Marshal.PtrToStructure<MINMAXINFO>(lParam);

        // Both are relative to the monitor's top-left, not to the desktop.
        mmi.ptMaxPosition.x = info.rcWork.left - info.rcMonitor.left;
        mmi.ptMaxPosition.y = info.rcWork.top - info.rcMonitor.top;
        mmi.ptMaxSize.x = info.rcWork.right - info.rcWork.left;
        mmi.ptMaxSize.y = info.rcWork.bottom - info.rcWork.top;

        Marshal.StructureToPtr(mmi, lParam, true);
    }

    /// <summary>
    /// Full bounds of the monitor the window is on, taskbar included. Sizing a foreground
    /// window to exactly this is what makes Windows treat it as fullscreen and let it cover
    /// the taskbar — the same thing the browser does for F11.
    /// </summary>
    public static bool TryGetMonitorBounds(IntPtr hwnd, out int x, out int y, out int width, out int height)
    {
        x = y = width = height = 0;

        var monitor = MonitorFromWindow(hwnd, MONITOR_DEFAULTTONEAREST);
        if (monitor == IntPtr.Zero) return false;

        var info = new MONITORINFO { cbSize = (uint)Marshal.SizeOf<MONITORINFO>() };
        if (!GetMonitorInfo(monitor, ref info)) return false;

        x = info.rcMonitor.left;
        y = info.rcMonitor.top;
        width = info.rcMonitor.right - info.rcMonitor.left;
        height = info.rcMonitor.bottom - info.rcMonitor.top;
        return true;
    }

    public static bool TryGetWindowBounds(IntPtr hwnd, out int x, out int y, out int width, out int height)
    {
        x = y = width = height = 0;
        if (!GetWindowRect(hwnd, out var r)) return false;

        x = r.left;
        y = r.top;
        width = r.right - r.left;
        height = r.bottom - r.top;
        return true;
    }

    /// <summary>
    /// Positions in physical pixels. WPF's Left/Top/Width/Height are device-independent units,
    /// which go wrong across monitors with different scaling.
    /// </summary>
    public static void SetBounds(IntPtr hwnd, int x, int y, int width, int height) =>
        SetWindowPos(hwnd, IntPtr.Zero, x, y, width, height, SWP_NOZORDER | SWP_FRAMECHANGED);

    private const uint MONITOR_DEFAULTTONEAREST = 2;
    private const uint SWP_NOZORDER = 0x0004;
    private const uint SWP_FRAMECHANGED = 0x0020;

    [StructLayout(LayoutKind.Sequential)]
    private struct POINT { public int x, y; }

    [StructLayout(LayoutKind.Sequential)]
    private struct RECT { public int left, top, right, bottom; }

    [StructLayout(LayoutKind.Sequential)]
    private struct MINMAXINFO
    {
        public POINT ptReserved;
        public POINT ptMaxSize;
        public POINT ptMaxPosition;
        public POINT ptMinTrackSize;
        public POINT ptMaxTrackSize;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct MONITORINFO
    {
        public uint cbSize;
        public RECT rcMonitor;
        public RECT rcWork;
        public uint dwFlags;
    }

    [DllImport("user32.dll")]
    private static extern IntPtr MonitorFromWindow(IntPtr hwnd, uint dwFlags);

    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    private static extern bool GetMonitorInfo(IntPtr hMonitor, ref MONITORINFO lpmi);

    [DllImport("user32.dll", SetLastError = true)]
    private static extern bool GetWindowRect(IntPtr hWnd, out RECT lpRect);

    [DllImport("user32.dll", SetLastError = true)]
    private static extern bool SetWindowPos(IntPtr hWnd, IntPtr after, int x, int y, int cx, int cy, uint flags);
}
