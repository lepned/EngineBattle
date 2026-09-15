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

The window is borderless (`WindowStyle="None"`), and EngineBattle's own MudBlazor app bar acts
as the title bar. The window buttons are injected by `TitleBarScript.cs` rather than added to
`MainLayout.razor`, so the web app stays unaware of the shell and a browser session never
renders window buttons that do nothing.

Several things here look odd without the reason:

- **Dragging goes through WebView2 web messages**, not `WindowChrome`'s caption area. The
  WebView2 control is an `HwndHost`; it takes mouse input first, so the host window never
  hit-tests a caption region. The page reports the gesture and the shell starts a native move
  loop, which keeps snapping and restore-on-drag working.
- **`BorderlessWindow.cs` handles `WM_GETMINMAXINFO`.** A borderless window maximises to the
  full monitor rectangle instead of the work area, putting the bottom of the page behind the
  taskbar — and still inside the viewport, so page code measuring `innerHeight` believes that
  hidden strip is usable.
- **The 6px margin on `RootHost` is the resize gutter** (0 when maximised or fullscreen). The
  WebView2's HWND would otherwise cover every window edge, leaving nothing to grab.
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
| `Ctrl+Shift+L` | Server output window |
| `F12` / `Ctrl+Shift+I` | DevTools |

Browser accelerator keys are disabled so they cannot swallow EngineBattle's own shortcuts,
which is why F11 and F12 are routed explicitly from the injected script.

DevTools is enabled in Release on purpose: EngineBattle is a developer-facing tool, and without
an address bar this is the only way to inspect a layout problem that only reproduces here.

### Fullscreen and the navigation drawer

Entering fullscreen closes the MudBlazor drawer and leaving reopens it — but only if the shell
was the one that closed it, so a drawer you had already collapsed is not forced back open.

**This is the only part of the shell coupled to EngineBattle's DOM**, via the MudBlazor class
names `.mud-drawer--open` and the app bar's first button. If those change in a MudBlazor
upgrade, the drawer sync silently stops working — no errors, no broken layout, but no warning
either. Everything else is window management or positioned against the viewport.

## Files it writes

All under `%LOCALAPPDATA%\EngineBattle\`, never beside the executable, so the shell also works
from a read-only install directory:

| File | Purpose |
|---|---|
| `desktop-server.log` | The server's stdout/stderr — a WinExe has no console. Truncated per run |
| `desktop-window.json` | Window size, position, maximised state |
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

- **Snap Layouts** (hovering the maximise button) do not appear: the button is in the page, not
  the non-client area. It needs `WM_NCHITTEST` returning `HTMAXBUTTON` for the button's
  rectangle, which means the page reporting its bounds to the shell.
- **Auto-hide taskbar** is untested. A maximised borderless window can stop it un-hiding; the
  usual workaround is a 1px inset.
- **Multi-monitor with mixed DPI** is untested for moving a maximised window between screens.

## If this is ever packaged for the Microsoft Store

`WindowsPackageType=None` → MSIX is a build-property change, not a code change, and
`runFullTrust` covers spawning engine processes and binding localhost. The real blocker is
elsewhere: an MSIX install directory is read-only, and EngineBattle writes user state into its
own install tree (`Data/globalSettings.json`, `wwwroot/tournament.json`, `PuzzleConfig.json`,
`EretConfig.json`). That is the same problem the `feature/user-data-dir` work addresses.
