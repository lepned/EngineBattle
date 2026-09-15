using System.Runtime.InteropServices;

namespace EngineBattle.Desktop;

/// <summary>
/// Plain Win32 single-instance handshake: a mutex decides who owns the app, a named event lets
/// a second launch raise the first window, and a second event acknowledges it.
///
/// The acknowledgement matters. Without it, an instance that still holds the mutex but has no
/// usable window — hung, or mid-teardown — silently swallows every later launch, and the app
/// simply stops responding to double-clicks with nothing to see. On no acknowledgement the new
/// instance starts normally instead, so the app can always be launched.
///
/// Deliberately avoids AppInstance, whose activation redirection needs the package identity
/// this unpackaged shell does not have.
/// </summary>
internal static class SingleInstance
{
    private const string MutexName = @"Local\EngineBattle.Desktop.Instance";
    private const string ActivateEventName = @"Local\EngineBattle.Desktop.Activate";
    private const string AcknowledgeEventName = @"Local\EngineBattle.Desktop.Activated";

    /// <summary>Long enough for a busy but healthy instance, short enough not to feel stuck.</summary>
    private static readonly TimeSpan AcknowledgeTimeout = TimeSpan.FromSeconds(2);

    private static Mutex? _mutex;
    private static EventWaitHandle? _activateEvent;
    private static EventWaitHandle? _acknowledgeEvent;

    public static bool TryAcquire()
    {
        _mutex = new Mutex(initiallyOwned: true, MutexName, out bool createdNew);
        return createdNew;
    }

    /// <summary>
    /// Asks a running instance to show itself. Returns false when nothing acknowledged, meaning
    /// the caller should carry on and start its own window.
    /// </summary>
    public static bool TryHandOffToExistingInstance()
    {
        try
        {
            if (!EventWaitHandle.TryOpenExisting(ActivateEventName, out var activate))
                return false;

            using (activate)
            {
                if (!EventWaitHandle.TryOpenExisting(AcknowledgeEventName, out var acknowledged))
                    return false;

                using (acknowledged)
                {
                    acknowledged.Reset();
                    activate.Set();
                    return acknowledged.WaitOne(AcknowledgeTimeout);
                }
            }
        }
        catch
        {
            // Nothing listening, or the other instance is going away.
            return false;
        }
    }

    /// <summary>Runs a background waiter that invokes <paramref name="onActivate"/> per signal.</summary>
    public static void ListenForActivation(Action onActivate)
    {
        _activateEvent = new EventWaitHandle(false, EventResetMode.AutoReset, ActivateEventName);
        _acknowledgeEvent = new EventWaitHandle(false, EventResetMode.ManualReset, AcknowledgeEventName);

        var thread = new Thread(() =>
        {
            while (true)
            {
                try
                {
                    _activateEvent.WaitOne();
                    onActivate();
                    _acknowledgeEvent.Set();
                }
                catch
                {
                    // The window is gone and its dispatcher has shut down. Stop listening and,
                    // crucially, stop acknowledging, so the next launch takes over instead of
                    // handing off to an instance that cannot show anything.
                    return;
                }
            }
        })
        { IsBackground = true, Name = "EngineBattle activation listener" };

        thread.Start();
    }

    [DllImport("user32.dll")]
    internal static extern bool SetForegroundWindow(IntPtr hWnd);

    [DllImport("user32.dll")]
    internal static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);

    internal const int SW_RESTORE = 9;
}
