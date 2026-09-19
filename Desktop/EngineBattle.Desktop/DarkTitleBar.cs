using System.Runtime.InteropServices;
using System.Windows;
using System.Windows.Interop;

namespace EngineBattle.Desktop;

/// <summary>
/// Asks the desktop window manager for a dark title bar and border, so the frame matches
/// EngineBattle's own dark interface instead of the system light theme.
///
/// WPF has no dark theme of its own and never will for these system-drawn parts; this DWM
/// attribute is what every app uses. It is a no-op on Windows versions that do not know the
/// attribute, so the window simply keeps the light frame there.
/// </summary>
internal static class DarkTitleBar
{
    // 20 from Windows 10 2004 on; 19 in the 1809-1909 window, where it had a different number.
    private const int DwmwaUseImmersiveDarkMode = 20;
    private const int DwmwaUseImmersiveDarkModeBefore20H1 = 19;

    /// <summary>Call once the window has a handle (SourceInitialized or later).</summary>
    public static void Apply(Window window)
    {
        var hwnd = new WindowInteropHelper(window).Handle;
        if (hwnd == IntPtr.Zero) return;
        Apply(hwnd);
    }

    public static void Apply(IntPtr hwnd)
    {
        var on = 1;
        try
        {
            if (DwmSetWindowAttribute(hwnd, DwmwaUseImmersiveDarkMode, ref on, sizeof(int)) != 0)
                DwmSetWindowAttribute(hwnd, DwmwaUseImmersiveDarkModeBefore20H1, ref on, sizeof(int));
        }
        catch (DllNotFoundException) { /* no dwmapi: nothing to do */ }
        catch (EntryPointNotFoundException) { /* older dwmapi */ }
    }

    [DllImport("dwmapi.dll", PreserveSig = true)]
    private static extern int DwmSetWindowAttribute(IntPtr hwnd, int attribute, ref int value, int size);
}
