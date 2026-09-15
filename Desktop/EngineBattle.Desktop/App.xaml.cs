using System.Windows;

namespace EngineBattle.Desktop;

public partial class App : Application
{
    protected override void OnStartup(StartupEventArgs e)
    {
        // Each instance would start its own EngineBattle.exe on its own port, giving the user
        // two independent servers over the same config files. Hand off to the first one.
        // Hand off only if a running instance actually acknowledges. If nothing answers, the
        // mutex holder cannot show a window, and exiting here would make the app look dead to
        // every double-click.
        if (!SingleInstance.TryAcquire() && SingleInstance.TryHandOffToExistingInstance())
        {
            Shutdown();
            return;
        }

        base.OnStartup(e);

        var window = new MainWindow(e.Args);
        MainWindow = window;
        SingleInstance.ListenForActivation(window.BringToFront);
        window.Show();
    }
}
