module ChessLibrary.TablebaseProbe

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text.RegularExpressions

/// Represents the parsed tablebase result from Fathom
type TablebaseResult = {
    Fen: string option
    Wdl: string option
    Dtz: string option
    WinningMoves: string list
    DrawingMoves: string list
    LosingMoves: string list
}

// Compiled regex to match lines like: [FieldName "value"]
let private headerRegex = Regex(@"\[(\w+)\s+""([^""]*)""\]", RegexOptions.Compiled)

/// Splits a comma-separated moves string into a list of trimmed moves
let parseMoves (value: string) =
    if String.IsNullOrWhiteSpace(value) then []
    else
        value.Split(',')
        |> Array.map (fun s -> s.Trim())
        |> Array.filter (fun s -> not (String.IsNullOrEmpty s))
        |> Array.toList

/// Parses the full Fathom tablebase output into a TablebaseResult record
let parse (input: string) : TablebaseResult =
    // Define an initial result with empty values
    let initial = {
        Fen = None
        Wdl = None
        Dtz = None
        WinningMoves = []
        DrawingMoves = []
        LosingMoves = []
    }
    input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold (fun acc line ->
        let m = headerRegex.Match(line)
        if m.Success then
            let key = m.Groups.[1].Value
            let value = m.Groups.[2].Value
            match key with
            | "FEN"           -> { acc with Fen = Some value }
            | "WDL"           -> { acc with Wdl = Some value }
            | "DTZ"           -> { acc with Dtz = Some value }
            | "WinningMoves"  -> { acc with WinningMoves = parseMoves value }
            | "DrawingMoves"  -> { acc with DrawingMoves = parseMoves value }
            | "LosingMoves"   -> { acc with LosingMoves = parseMoves value }
            | _               -> acc
        else acc
    ) initial

/// Ensures that the specified file has executable permissions (Linux/macOS)
let ensureExecutablePermissions (filePath: string) =
    try
        let startInfo =
            ProcessStartInfo(
                FileName = "chmod",
                Arguments = sprintf "+x \"%s\"" filePath,
                UseShellExecute = false,
                CreateNoWindow = true)
        use proc = new Process(StartInfo = startInfo)
        proc.Start() |> ignore
        proc.WaitForExit()
    with ex ->
        Console.Error.WriteLine(sprintf "Failed to set executable permissions: %s" ex.Message)

/// Determines the correct Fathom executable path based on the current OS
let getFathomExecutablePath () =
    let basePath = AppDomain.CurrentDomain.BaseDirectory
    let exePath =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            Path.Combine(basePath, "Tools", "fathom.exe")
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
            Path.Combine(basePath, "Tools", "fathom.linux")
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
            Path.Combine(basePath, "Tools", "fathom.macosx")
        else
            failwith "Unsupported OS platform."

    // For Linux and macOS, ensure the file has execute permissions
    if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) ||
       RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
        ensureExecutablePermissions exePath

    //check if exePath exists
    if not (File.Exists exePath) then
        failwithf "Fathom executable not found at path: %s" exePath
    exePath

/// Stands in for the prober path until getFathomExecutablePath has returned one.
let private unresolvedProber = "(not resolved)"

/// <summary>
/// Two guards, not one, and the difference matters.
///
/// A prober that CANNOT RUN is a standing condition: every position from here on will go
/// unadjudicated, and the user needs to be told once. A TIMEOUT is not - a single slow spawn
/// under load, or a cold tablebase directory on a spinning disk, costs one position and nothing
/// more. Sharing a latch between them meant one early timeout both claimed that adjudication was
/// over when it was not, and then swallowed the real failure if the prober later stopped working
/// for good - which is the exact case this reporting exists for.
/// </summary>
let private proberUnusableReported = ref 0
let private probeTimeoutReported = ref 0

/// <summary>
/// Clears both, so a long-lived host reports again for the next tournament. The WebGUI runs many
/// of them in one process, with different TablebaseDirectory settings; without this, a failure
/// reported during the first run would stay silent for every run after it. Same convention as
/// resetPrintedEngines, and called from the same place.
/// </summary>
let resetProbeReports () =
    probeTimeoutReported.Value <- 0
    proberUnusableReported.Value <- 0

/// <summary>
/// Says once why the tablebase prober is not answering.
///
/// This runs for every position that reaches the adjudication threshold, so a prober that cannot
/// run would either print for every move of every game or - as it did before - say nothing at
/// all. Silence is the worse of the two: a user who has configured TablebaseDirectory gets no
/// adjudication and no reason, and everything looks like it is working.
///
/// The Apple Silicon line is here because the bundled macOS prober is an x86_64 Mach-O binary and
/// needs Rosetta 2 to start. The tablebase FILES are plain data and are fine on any machine; it
/// is only this program that cannot run natively there.
/// </summary>
let private reportProberUnusable (exePath: string) (reason: string) =
    if System.Threading.Interlocked.Exchange(proberUnusableReported, 1) = 0 then
        Console.Error.WriteLine(
            sprintf "Tablebase probe failed, so positions will not be adjudicated from tablebases: %s" reason)
        // Only when we got that far: when resolving the path is what failed, the reason above
        // already names it, and a second line saying "(not resolved)" reads as a contradiction.
        if exePath <> unresolvedProber then
            Console.Error.WriteLine(sprintf "  prober: %s" exePath)
        if RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
           && RuntimeInformation.ProcessArchitecture = Architecture.Arm64 then
            Console.Error.WriteLine(
                "  The bundled macOS prober is x86_64. On Apple Silicon it needs Rosetta 2: softwareupdate --install-rosetta")

/// One line for the first slow probe, and nothing after it: the position is lost, the run is not.
let private reportProbeTimeout (timeoutMs: int) =
    if System.Threading.Interlocked.Exchange(probeTimeoutReported, 1) = 0 then
        Console.Error.WriteLine(
            sprintf "Tablebase probe timed out after %d ms; that position was not adjudicated. Later timeouts are not repeated."
                timeoutMs)

/// How one probe ended.
type ProbeOutcome =
    | Answer of string
    /// It ran and gave no answer for this position (castling rights, a material combination the
    /// directory lacks, a prober that failed); the exit code says which.
    | NoAnswer of exitCode: int
    | TimedOut
    | Failed of string

/// The first probe of a run opens every table file; on a cold disk that took 4 s.
let firstProbeTimeoutMs = 10_000
let probeTimeoutMs = 3_000

/// Most pieces of any Syzygy table in the directory, from the file names (KBBBBvK.rtbw = 6); 0
/// when it holds none.
let largestTable (tablebasePath: string) =
    try
        Directory.EnumerateFiles(tablebasePath, "*.rtbw")
        |> Seq.map (fun f -> (Path.GetFileNameWithoutExtension f).Replace("v", "").Length)
        |> Seq.fold max 0
    with _ -> 0

/// Runs Fathom without holding a thread: its output is read on a thread of its own (a pipe read on
/// the pool blocks a pool thread) and the exit is awaited, not waited for.
let runFathomAsync (prober: string) (tablebasePath: string) (fen: string) (timeoutMs: int) (cancel: Threading.CancellationToken) : Async<ProbeOutcome> = async {
    try
        let startInfo = ProcessStartInfo(FileName = prober, UseShellExecute = false, CreateNoWindow = true,
                                         RedirectStandardOutput = true, RedirectStandardError = false)
        startInfo.ArgumentList.Add($"--path={tablebasePath}")
        startInfo.ArgumentList.Add(fen)
        use proc = new Process(StartInfo = startInfo)
        if not (proc.Start()) then return Failed "the process could not be started"
        else
            // drained while it runs: Fathom blocks writing once the pipe buffer is full
            let output =
                Threading.Tasks.Task.Factory.StartNew((fun () -> try proc.StandardOutput.ReadToEnd() with _ -> ""),
                                                      Threading.Tasks.TaskCreationOptions.LongRunning)
            use cts = Threading.CancellationTokenSource.CreateLinkedTokenSource(cancel)
            cts.CancelAfter timeoutMs
            let! exited = async {
                try
                    do! proc.WaitForExitAsync(cts.Token) |> Async.AwaitTask
                    return true
                with _ -> return false }
            if exited then
                let! text = output |> Async.AwaitTask
                return if proc.ExitCode = 0 && not (String.IsNullOrWhiteSpace text) then Answer text else NoAnswer proc.ExitCode
            else
                try proc.Kill true with _ -> ()
                return TimedOut
    with ex -> return Failed ex.Message }

/// What the prober agent knows: per directory its largest table and whether a probe has opened
/// its tables this run, the resolved prober, and which failures have been reported.
type ProberState =
    { Largest: Map<string, int>
      Warm: Set<string>
      Prober: string option
      NoAnswerReported: bool }

let private initial = { Largest = Map.empty; Warm = Set.empty; Prober = None; NoAnswerReported = false }

type private ProberMessage =
    | StartRun of useTablebases: bool * men: int * path: string
    | Probe of path: string * fen: string * pieces: int * Threading.CancellationToken * AsyncReplyChannel<string option>

let private key (path: string) = path.TrimEnd('/', '\\').ToLowerInvariant()

/// The one owner of tablebase probing: probes run one at a time, so the warm-up opens the tables
/// before the first game's probe, and no state is shared between threads.
let private agent = MailboxProcessor<ProberMessage>.Start(fun inbox ->
    let largestOf (state: ProberState) path =
        match state.Largest.TryFind (key path) with
        | Some n -> state, n
        | None -> let n = largestTable path in { state with Largest = state.Largest.Add(key path, n) }, n
    // resolved once it has worked (on Linux/macOS that includes a chmod); a failure is tried again
    let proberOf (state: ProberState) =
        match state.Prober with
        | Some p -> state, Ok p
        | None ->
            try let p = getFathomExecutablePath () in { state with Prober = Some p }, Ok p
            with ex -> state, Error ex.Message
    let handle (state: ProberState) message = async {
        match message with
        | StartRun (useTablebases, men, path) ->
            resetProbeReports ()
            // the directory is read again: tables may have been added since the last run
            let state = { state with NoAnswerReported = false; Warm = Set.empty; Largest = state.Largest.Remove (key path) }
            if useTablebases && not (String.IsNullOrEmpty path) && Directory.Exists path then
                let state, largest = largestOf state path
                if largest < men then
                    Console.Error.WriteLine(
                        sprintf "Tablebases in %s go up to %d pieces; positions with more are not adjudicated from tablebases." path largest)
                match proberOf state with
                | state, Ok prober when largest >= 3 ->
                    // three pieces: every Syzygy set has them; queued probes wait for it
                    let! outcome = runFathomAsync prober path "8/8/8/4k3/8/8/4P3/4K3 w - - 0 1" firstProbeTimeoutMs Threading.CancellationToken.None
                    match outcome with
                    | Answer _ -> return { state with Warm = state.Warm.Add (key path) }
                    | _ -> return state
                | state, _ -> return state
            else return state
        | Probe (path, fen, pieces, cancel, reply) ->
            let state, largest = largestOf state path
            if pieces > largest || cancel.IsCancellationRequested then
                reply.Reply None
                return state
            else
                match proberOf state with
                | state, Error reason ->
                    reportProberUnusable unresolvedProber reason
                    reply.Reply None
                    return state
                | state, Ok prober ->
                    let timeoutMs = if state.Warm.Contains (key path) then probeTimeoutMs else firstProbeTimeoutMs
                    let! outcome = runFathomAsync prober path fen timeoutMs cancel
                    let state =
                        match outcome with
                        | Answer _ | NoAnswer _ -> { state with Warm = state.Warm.Add (key path) }
                        | _ -> state
                    match outcome with
                    | Answer text ->
                        reply.Reply (Some text)
                        return state
                    | NoAnswer code ->
                        if not state.NoAnswerReported then
                            Console.Error.WriteLine(
                                sprintf "Tablebase probe gave no answer (exit code %d) for %s; such positions are played on. Later ones are not repeated." code fen)
                        reply.Reply None
                        return { state with NoAnswerReported = true }
                    | TimedOut ->
                        if not cancel.IsCancellationRequested then reportProbeTimeout timeoutMs
                        reply.Reply None
                        return state
                    | Failed reason ->
                        reportProberUnusable prober reason
                        reply.Reply None
                        return state }
    // a failure answers the waiting game and keeps the agent: a dead one would hang every probe
    let rec loop (state: ProberState) = async {
        let! message = inbox.Receive()
        let! next = async {
            try return! handle state message
            with ex ->
                match message with
                | Probe (_, _, _, _, reply) -> try reply.Reply None with _ -> ()
                | _ -> ()
                Console.Error.WriteLine(sprintf "Tablebase prober: %s" ex.Message)
                return state }
        return! loop next }
    loop initial)

/// One probe: None when there is no answer (too many pieces for the tables, or none given).
let probeAsync (tablebasePath: string) (fen: string) (pieces: int) (cancel: Threading.CancellationToken) : Async<string option> =
    agent.PostAndAsyncReply(fun reply -> Probe (tablebasePath, fen, pieces, cancel, reply))

/// A new run: reports cleared and, with tablebase adjudication on, the tables opened by one probe
/// before any game's, so the slow first opening happens before a game needs them.
let startRun (useTablebases: bool) (men: int) (tablebasePath: string) =
    agent.Post (StartRun (useTablebases, men, tablebasePath))
