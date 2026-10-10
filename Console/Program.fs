namespace ConsoleApp

open System
open ChessLibrary.RuntimeUtilities

/// The processes attached to this console: 1 when the program has a console of its own (started
/// from Explorer rather than from a shell).
module ConsoleWindow =
  [<System.Runtime.InteropServices.DllImport("kernel32.dll")>]
  extern uint32 GetConsoleProcessList(uint32[] processList, uint32 processCount)
  [<System.Runtime.InteropServices.DllImport("kernel32.dll")>]
  extern nativeint GetConsoleWindow()
  [<System.Runtime.InteropServices.DllImport("user32.dll")>]
  extern bool IsWindowVisible(nativeint hWnd)

/// eb-cli: `match` goes to MatchMode (fastchess's command line); everything else to Cli.App with the
/// commands of Registry.
module Program =

  /// Double-clicked from Explorer, the window would close before the command list could be read.
  /// Only a window someone can see: a program that starts this one with a hidden console
  /// (CreateNoWindow) also leaves it alone on its console, and nobody could press Enter there.
  let waitIfOwnConsole () =
    if OperatingSystem.IsWindows() && not Console.IsInputRedirected then
      try
        let window = ConsoleWindow.GetConsoleWindow()
        if ConsoleWindow.GetConsoleProcessList(Array.zeroCreate 4, 4u) <= 1u
           && window <> 0n && ConsoleWindow.IsWindowVisible window then
          printfn "Press Enter to close."
          Console.ReadLine() |> ignore
      with _ -> ()

  [<EntryPoint>]
  let main argv =
    // Invariant culture before anything formats or parses a number — see WebGUI/Program.cs
    // for the reproduction. This side writes the PGN files, where a comma decimal would not
    // just look wrong but corrupt what every other tool reads back.
    Globalization.CultureInfo.DefaultThreadCurrentCulture <- Globalization.CultureInfo.InvariantCulture
    Globalization.CultureInfo.DefaultThreadCurrentUICulture <- Globalization.CultureInfo.InvariantCulture
    Threading.Thread.CurrentThread.CurrentCulture <- Globalization.CultureInfo.InvariantCulture
    Threading.Thread.CurrentThread.CurrentUICulture <- Globalization.CultureInfo.InvariantCulture
    Console.OutputEncoding <- Text.Encoding.UTF8

    // `match`, or a command line that starts with a fastchess option (so a tool that runs matches
    // can run this program; a mistyped option then gets match's error and suggestion). MatchMode
    // owns stdout, so it goes before anything prints.
    match List.ofArray argv with
    | first :: rest when first = "match" || (first.StartsWith "-" && not (List.contains first [ "-h"; "--help"; "--version" ])) ->
        let viaVerb = first = "match"
        MatchMode.run (if viaVerb then rest else List.ofArray argv) viaVerb
    | tokens ->
        ConsoleUtils.originalColor <- Console.ForegroundColor
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> Commands.Gui.stop ())
        let code = Cli.App.run Registry.all tokens
        if tokens.IsEmpty then waitIfOwnConsole ()
        code
