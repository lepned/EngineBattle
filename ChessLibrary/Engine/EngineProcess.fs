namespace ChessLibrary

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open TypesDef.CoreTypes

/// The process plumbing both engine wrappers share (Engine.fs): the engine process itself
/// (Transport), what its stderr is kept as, how its I/O is logged to a file and how the
/// diagnostics read. Nothing here knows UCI from Winboard - that is EngineWire's - or what a
/// search is.
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

    /// Opens a new engine_<name>_<timestamp>.log in `dir`. Two engines of one name started in the
    /// same second (compare, dual analysis) used to get the same file, and the second failed to
    /// start: a name taken already gets _2, _3, ...
    static member internal OpenAt(dir: string, engineName: string, ts: string) =
      if not (Directory.Exists dir) then Directory.CreateDirectory(dir) |> ignore
      let safeName = engineName.Replace(" ", "_").Replace("/", "_").Replace("\\", "_")
      let rec create n =
        let path = Path.Combine(dir, $"""engine_{safeName}_{ts}{(if n = 1 then "" else $"_{n}")}.log""")
        try path, new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Read)
        with :? IOException when n < 100 && File.Exists path -> create (n + 1)
      let path, stream = create 1
      path, new IoLog(new StreamWriter(stream))

    /// Opens a new logs/engine_<name>_<timestamp>.log under the current directory.
    static member Open(engineName: string) =
      IoLog.OpenAt(Path.Combine(Environment.CurrentDirectory, "logs"), engineName, DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss"))

    member _.Write(direction: string, text: string) =
      channel.Writer.TryWrite({ At = DateTime.Now; Direction = direction; Text = text }) |> ignore

    interface IDisposable with
      member _.Dispose() =
        channel.Writer.TryComplete() |> ignore
        try drained.Wait(5000) |> ignore with _ -> ()
        try writer.Dispose() with _ -> ()

  // ── The engine process ─────────────────────────────────────────────────────────────────────────

  /// How the engine's stdout is read, always on the process's reader thread: into a channel that
  /// completes when the output ends. A channel read can be cancelled cleanly; a cancelled
  /// StandardOutput read stays pending on the pipe.
  type OutputMode =
    /// The analysis wrapper.
    | Lines of ChannelWriter<string>
    /// Each line with the Stopwatch timestamp of its reading, for a clock that must not be
    /// charged for how long the line waited to be handled.
    | Stamped of ChannelWriter<struct (int64 * string)>

  /// Reads a pipe line by line on a thread of its own until it ends, then calls `onEnd`. Not the
  /// pool: Windows opens process pipes without overlapped I/O, so an async read blocks a pool
  /// thread for as long as the engine is silent. Process.BeginOutputReadLine did that for every
  /// engine's stdout and stderr, and ten games starved the pool for seconds: bestmoves were read
  /// late and charged to the engine's clock.
  let private startReader (name: string) (reader: StreamReader) (onLine: string -> unit) (onEnd: unit -> unit) =
    let run () =
      try
        try
          let mutable line = reader.ReadLine()
          while not (isNull line) do
            (try onLine line with _ -> ())
            line <- reader.ReadLine()
        with _ -> () // the process was disposed under the read
      finally
        // Process.Close leaves a synchronously read stream open
        try reader.Dispose() with _ -> ()
        try onEnd () with _ -> ()
    Thread(run, 256 * 1024, IsBackground = true, Name = name).Start()

  /// One engine process: started in the engine's own folder with its arguments, stdin written a
  /// line at a time under a lock (LF on every platform, flushed at once), stderr kept in the
  /// wrapper's StderrRing, and the exit code captured when it goes. The ring belongs to the
  /// wrapper, not the process, so a restarted engine keeps the history of the one that died. What
  /// the wrappers say about each of these differs, so they pass it in: `note` for the
  /// working-directory line, `onStderr` for each stderr line, `onExited` when the process ends
  /// (with its code when readable).
  [<AllowNullLiteral>]
  type Transport(config: EngineConfig, arguments: string, stderr: StderrRing, note: string -> unit, onStderr: string -> unit, onExited: int option -> unit) =
    let proc = new Process()
    let writeLock = obj ()
    [<VolatileField>]
    let mutable exitCode : int option = None

    /// Starts the process; false when it could not be started.
    member _.Start(mode: OutputMode) =
      proc.StartInfo.FileName <- config.Path
      proc.StartInfo.UseShellExecute <- false
      proc.StartInfo.RedirectStandardInput <- true
      proc.StartInfo.RedirectStandardOutput <- true
      proc.StartInfo.RedirectStandardError <- true
      // The engine's own folder, so it writes its logs and caches there rather than beside the app.
      match workingDirectory config with
      | Some dir ->
          proc.StartInfo.WorkingDirectory <- dir
          note $"Working directory set to: {dir}"
      | None ->
          note $"WARNING: Could not set working directory. Path: {config.Path}, Dir: {Path.GetDirectoryName(config.Path)}"
      if not (String.IsNullOrEmpty arguments) then proc.StartInfo.Arguments <- arguments
      proc.Exited.Add(fun _ ->
        let code = try Some proc.ExitCode with _ -> None
        exitCode <- code
        try onExited code with _ -> ())
      proc.EnableRaisingEvents <- true
      let onError (line: string) =
        if not (String.IsNullOrEmpty line) then
          stderr.Add line
          onStderr line
      let onOutput, onEnd =
        match mode with
        | Lines writer -> (fun line -> writer.TryWrite line |> ignore), (fun () -> writer.TryComplete() |> ignore)
        | Stamped writer ->
            (fun line -> writer.TryWrite(struct (Stopwatch.GetTimestamp(), line)) |> ignore), (fun () -> writer.TryComplete() |> ignore)
      if proc.Start() then
        proc.StandardInput.NewLine <- "\n"
        proc.StandardInput.AutoFlush <- true
        startReader $"{config.Name} stderr" proc.StandardError onError ignore
        startReader $"{config.Name} stdout" proc.StandardOutput onOutput onEnd
        true
      else false

    member _.Process = proc
    /// The exit code once the Exited event has delivered it.
    member _.ExitCode = exitCode
    /// Throws when the process was never started or has been disposed, as Process.HasExited does.
    member _.HasExited = proc.HasExited

    member _.WriteLine(line: string) =
      lock writeLock (fun () -> proc.StandardInput.WriteLine line)

    /// The exit code, read from the process when the Exited event has not delivered it yet.
    member _.ExitCodeText() =
      match exitCode with
      | Some c -> string c
      | None ->
          try if proc.HasExited then string proc.ExitCode else "?"
          with _ -> "?"

    /// After a read returned null: the engine closed its output, which it does as it exits. The
    /// exit can lag the closed pipe by a moment; wait for it so a reason can name the exit.
    member _.ExitedAfterEndOfOutput() =
      try proc.HasExited || proc.WaitForExit 2000
      with _ -> true
