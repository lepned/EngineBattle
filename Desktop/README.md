# EngineBattle Desktop (Windows)

A native window around the existing EngineBattle web UI. It starts `EngineBattle.exe`, waits
for it to serve HTTP, and shows it in a WebView2 — so EngineBattle itself keeps running
unchanged on Windows, Linux and macOS.

**This project has no compile-time reference to `WebGUI` or `ChessLibrary`.** The contract is a
process contract: launch the server, poll until it answers, navigate. That is deliberate — a
problem in this Windows-only shell can never break the cross-platform build.

## Build and run

```bash
dotnet build Desktop/EngineBattle.Desktop -c Release
Desktop/EngineBattle.Desktop/bin/Release/net10.0-windows/EngineBattleDesktop.exe
```

It is **not** part of `EngineBattle.sln`, and must stay that way: it targets
`net10.0-windows`, which would break `dotnet build` on the `ubuntu-latest` jobs in
`.github/workflows/release.yml`. Use `Desktop/EngineBattle.Desktop.slnx` instead.

### Finding the server

In order:

1. `--server <path>`
2. `ENGINEBATTLE_EXE` environment variable
3. `EngineBattle.exe` beside `EngineBattleDesktop.exe` — the released layout
4. A source checkout: `dotnet run --project WebGUI`

There is deliberately no probe of `publish/`. A stale publish folder silently shadowing the
working tree is a trap, not a convenience.

**In a source checkout the server is launched once, at shell startup.** Any change to WebGUI
needs a full shell restart to take effect. In a released build this does not apply — the
published exe is launched directly, and answers in well under a second.

The child is started with its working directory set to its own folder, matching double-click
behaviour: the server resolves tournament, engine and log paths against the current directory.
It is also attached to a Win32 job object, so it cannot outlive a crashed shell and leave a
port bound.

## What this required from EngineBattle

One thing, in `WebGUI/Program.cs`:

- `--no-browser` (or `ENGINEBATTLE_NO_BROWSER=1`) suppresses the browser auto-launch.
- The resolved startup URL is always printed as `EngineBattle startup URL: <url>`, so the shell
  can open the user's configured startup page instead of `/`. The server stays the single
  source of truth for that setting — the shell does not parse `globalSettings.json`.

## Window behaviour

The window has the ordinary Windows title bar and border (`WindowStyle="SingleBorderWindow"`),
so it drags, snaps and resizes like any other app. `DarkTitleBar.cs` asks the desktop window
manager for a dark frame. Below the title bar is a native WPF menu with the shell's own
commands; the app's navigation stays in its drawer:

| Menu | Items |
|---|---|
| ☰ | Show or hide the navigation drawer |
| File | New Tournament (the Tournament creator), Open Tournament (makes another tournament.json the current one; the old one is kept as `tournament_<date_time>.json`), Exit |
| View | Zoom In, Zoom Out, Reset Zoom (shows the current level), Reload, Full Screen |
| Tools | Server Output, Developer Tools |
| Help | About EngineBattle (shell version, WebView2 runtime, server URL) |

Reload drops the Blazor circuit and starts the page over; tournaments and analysis keep running
in the server. The zoom level is remembered between runs.

`TitleBarScript.cs` is injected into every page, but draws nothing of its own. It routes the
shortcuts (keys pressed inside the WebView never reach WPF), clicks the drawer toggle when the
menu asks for it, and hides the page's own toggle while the menu carries one. Being in the
shell rather than in `MainLayout.razor`, it leaves the web app unaware of the shell, and a
browser session unaffected.

Several things here look odd without the reason:

- **`BorderlessWindow.cs` handles `WM_GETMINMAXINFO`** only while the window is borderless, in
  fullscreen. A borderless window maximises to the full monitor rectangle instead of the work
  area; a framed one is maximised correctly by Windows.
- **WPF cannot draw over an `HwndHost`**, which is why the loading and error overlay only works
  while the WebView is collapsed.
- **`Window.Icon` is deliberately not set.** A `BitmapImage` built from a multi-size `.ico`
  resolves to its 16x16 frame, which Windows then scales up for the taskbar. Leaving it unset
  uses the exe's embedded icon, which keeps all four sizes.

### Keyboard

| Key | Action |
|---|---|
| `F11` | Fullscreen, covering the taskbar — as the browser does |
| `Esc` | Leave fullscreen |
| `Ctrl++` / `Ctrl+=` | Zoom in |
| `Ctrl+-` | Zoom out |
| `Ctrl+0` | Reset zoom |
| `F5` | Reload |
| `Ctrl+Shift+L` | Server output window |
| `F12` / `Ctrl+Shift+I` | DevTools |
| `Alt+F4` | Exit |

Browser accelerator keys are disabled so they cannot swallow EngineBattle's own shortcuts,
which is why these keys are routed explicitly from the injected script.

DevTools is enabled in Release on purpose: EngineBattle is a developer-facing tool, and without
an address bar this is the only way to inspect a layout problem that only reproduces here.

### Fullscreen and the navigation drawer

Fullscreen drops the frame and the menu (`WindowStyle` None, `ResizeMode` NoResize, restored on
the way out) and leaves the drawer exactly as the user set it; F11 or Esc brings the frame and
menu back.

**The drawer toggle is the only part of the shell coupled to EngineBattle's DOM**, via the
classes `.eb-drawer-toggle` and `.eb-drawer-close` in `MainLayout.razor`. If those change, the
menu's ☰ item silently stops working — no errors, no broken layout, but no warning either.
Everything else is window management.

## Files it writes

All under `%LOCALAPPDATA%\EngineBattle\`, never beside the executable, so the shell also works
from a read-only install directory:

| File | Purpose |
|---|---|
| `desktop-server.log` | The server's stdout/stderr — a WinExe has no console. Truncated per run |
| `desktop-window.json` | Window size, position, maximised state |
| `desktop-preferences.json` | The remembered zoom level |
| `WebView2/` | Browser profile and cache |

The server's own Serilog file stays where it always was, under its working directory.

## Why WPF and not WinUI 3

Built both. The shell is a window around a WebView2 and uses no XAML controls of its own:

| | WinUI 3 | WPF |
|---|---|---|
| Output | 149 MB, 332 files | 2.8 MB, 14 files |
| Includes | onnxruntime 21 MB, DirectML 18 MB, WinUI 15 MB | — |

WPF also has an official `BlazorWebView`, which WinUI 3 does not — so hosting Blazor components
in-process remains possible later. It was not done, because WebGUI is Blazor **Server**:
`/api/livefeed` needs a real HTTP listener, per-circuit service lifetimes would change across
105 components, and it would create the compile-time coupling this project avoids. Measured
server start is ~0.67s, so there is little to gain.

## Known limitations

- **Auto-hide taskbar** is untested. A maximised borderless window can stop it un-hiding; the
  usual workaround is a 1px inset.
- **Multi-monitor with mixed DPI** is untested for moving a maximised window between screens.

## If this is ever packaged for the Microsoft Store

`WindowsPackageType=None` → MSIX is a build-property change, not a code change, and
`runFullTrust` covers spawning engine processes and binding localhost. The real blocker is
elsewhere: an MSIX install directory is read-only, and EngineBattle writes user state into its
own install tree (`Data/globalSettings.json`, `wwwroot/tournament.json`, `PuzzleConfig.json`,
`EretConfig.json`). That is the same problem the `feature/user-data-dir` work addresses.
