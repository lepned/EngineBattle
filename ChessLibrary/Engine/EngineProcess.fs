namespace ChessLibrary

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open TypesDef.CoreTypes

/// The process plumbing both engine wrappers share (Engine.fs): what an engine's stderr is kept as,
/// how its I/O is logged to a file, where it runs and how the diagnostics read. Nothing here knows
/// UCI from Winboard - that is EngineWire's - or what a search is.
module internal EngineProcess =

  // ── Output text ───────────────────────────────────────────────────────────────────────────────

  /// Engines that colour their output (Ceres does) leave escape sequences in stderr, and those
  /// survive into the log where they make a stack trace harder to read than the thing it
  /// describes. Compiled once; stderr volume is low enough that this never shows up in a profile.
  let private ansiEscape = Regex(string (char 27) + @"\[[0-9;]*[A-Za-z]", RegexOptions.Compiled)

  let stripAnsi (line: string) =
    if String.IsNullOrEmpty line then line
    elif line.IndexOf '\u001b' < 0 then line   // the common case: nothing to strip
    else ansiEscape.Replace(line, "")

  /// Lines an engine prints on stdout when it has given up on initialization WITHOUT exiting.
  /// Ceres stays alive after a failed network/device load (so a GUI can send another setoption),
  /// which means "isready" never gets its "readyok" and a plain wait runs to its timeout (15 min
  /// in cmp, 2 h in tournaments) with the GPU idle. Seeing one of these ends the wait as a failure
  /// at once. Engines that exit instead are caught by the exit checks next to every wait.
  let private fatalInitMarkers = [| "Cannot initialize engine"; "No evaluator created" |]

  let isFatalInitLine (line: string) =
    not (String.IsNullOrEmpty line)
    && fatalInitMarkers |> Array.exists (fun m -> line.IndexOf(m, StringComparison.OrdinalIgnoreCase) >= 0)

  // ── What kind of engine, where it runs ─────────────────────────────────────────────────────────

  /// Lc0 and Ceres are recognised by their path, as they always have been.
  let pathMentions (engine: string) (config: EngineConfig) =
    Regex.Match(config.Path, engine, RegexOptions.IgnoreCase).Success

  /// The engine's own folder, so it writes its logs and caches there rather than beside the app;
  /// None when the path has no usable folder.
  let workingDirectory (config: EngineConfig) =
    let dir = Path.GetDirectoryName(config.Path)
    if not (String.IsNullOrWhiteSpace dir) && Directory.Exists dir then Some dir else None

  // ── Stderr ─────────────────────────────────────────────────────────────────────────────────────

  /// The engine's stderr, newest lines kept. A crash writes its stack last, so keeping the newest
  /// lines always preserves it, while a chatty engine cannot grow this without bound over a long
  /// tournament. Trimmed in blocks (keep 500, trim past 600) because RemoveRange(0, ...) shifts the
  /// whole list. The process writes from its stderr thread while a game thread reads the lot.
  type StderrRing() =
    let maxLines = 500
    let lines = ResizeArray<string>()
    let sync = obj ()
    let mutable dropped = 0

    member _.Add(line: string) =
      let line = stripAnsi line
      lock sync (fun () ->
        lines.Add line
        if lines.Count > maxLines + 100 then
          let excess = lines.Count - maxLines
          lines.RemoveRange(0, excess)
          dropped <- dropped + excess)

    member _.Snapshot() = lock sync (fun () -> lines.ToArray())

    /// Everything still held, with the stack of a dead engine at the end, for the log. The
    /// dropped count is reported rather than hidden, so a truncated history is visible instead of
    /// being mistaken for the whole story.
    member _.Diagnostics(name: string, exitCode: int option) =
      let snapshot, droppedNow = lock sync (fun () -> lines.ToArray(), dropped)
      let header =
        sprintf "Engine: %s | ExitCode: %s | Stderr lines: %d%s"
          name (match exitCode with Some c -> string c | None -> "N/A") snapshot.Length
          (if droppedNow > 0 then sprintf " (%d older lines dropped)" droppedNow else "")
      if snapshot.Length = 0 then header
      else
        sprintf "%s%s%s stderr:%s%s" header Environment.NewLine name Environment.NewLine
          (String.concat Environment.NewLine snapshot)

  // ── The per-engine I/O log ─────────────────────────────────────────────────────────────────────

  [<Struct>]
  type private IoEntry = { At: DateTime; Direction: string; Text: string }

  /// logs/engine_<name>_<time>.log, one "[HH:mm:ss.fff] >>> line" per command sent and
  /// "<<< line" per line read - the same file and format as before. What changed is who pays:
  /// the reader thread only queues the entry, and a background writer formats it and flushes once
  /// per batch instead of once per line. Under an info flood that was about a third of the
  /// analysis wrapper's CPU. Dispose writes out whatever is still queued.
  type IoLog private (writer: StreamWriter) =
    let channel = Channel.CreateUnbounded<IoEntry>(UnboundedChannelOptions(SingleReader = true))
    let drained =
      Task.Run(fun () ->
        task {
          let reader = channel.Reader
          let! more = reader.WaitToReadAsync()
          let mutable go = more
          while go do
            let mutable entry = Unchecked.defaultof<IoEntry>
            while reader.TryRead(&entry) do
              writer.Write('[')
              writer.Write(entry.At.ToString("HH:mm:ss.fff"))
              writer.Write("] ")
              writer.Write(entry.Direction)
              writer.Write(' ')
              writer.WriteLine(entry.Text)
            writer.Flush()
            let! more = reader.WaitToReadAsync()
            go <- more
        } :> Task)

    /// Opens logs/engine_<name>_<timestamp>.log under the current directory.
    static member Open(engineName: string) =
      let dir = Path.Combine(Environment.CurrentDirectory, "logs")
      if not (Directory.Exists dir) then Directory.CreateDirectory(dir) |> ignore
      let safeName = engineName.Replace(" ", "_").Replace("/", "_").Replace("\\", "_")
      let ts = DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")
      let path = Path.Combine(dir, $"engine_{safeName}_{ts}.log")
      path, new IoLog(new StreamWriter(path, append = true))

    member _.Write(direction: string, text: string) =
      channel.Writer.TryWrite({ At = DateTime.Now; Direction = direction; Text = text }) |> ignore

    interface IDisposable with
      member _.Dispose() =
        channel.Writer.TryComplete() |> ignore
        try drained.Wait(5000) |> ignore with _ -> ()
        try writer.Dispose() with _ -> ()
