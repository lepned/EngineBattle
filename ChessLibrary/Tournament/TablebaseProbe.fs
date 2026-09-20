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

/// Runs the Fathom executable with the given tablebase path and FEN, returning its output
let runFathom (tablebasePath: string) (fen: string) =
    let exePath = getFathomExecutablePath ()
    let arguments = sprintf "--path=\"%s\" \"%s\"" tablebasePath fen

    let startInfo =
        ProcessStartInfo(
            FileName = exePath,
            Arguments = arguments,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            CreateNoWindow = true)

    use proc = new Process(StartInfo = startInfo)
    proc.Start() |> ignore
    let output = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    output

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

/// Runs Fathom with a timeout, returning None on timeout or error
let runFathomSafe (tablebasePath: string) (fen: string) (timeoutMs:int) : string option =
    let mutable exePath = unresolvedProber
    try
        exePath <- getFathomExecutablePath ()
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- exePath
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- false // avoid potential pipe blocking on Linux
        // Build args safely across platforms
        startInfo.ArgumentList.Add($"--path={tablebasePath}")
        startInfo.ArgumentList.Add(fen)

        use proc = new Process(StartInfo = startInfo)
        if not (proc.Start()) then
            reportProberUnusable exePath "the process could not be started"
            None
        else
            // Drain stdout concurrently: reading only after WaitForExit deadlocks when
            // Fathom's output exceeds the pipe buffer (child blocks writing, wait times out).
            let outputTask = proc.StandardOutput.ReadToEndAsync()
            if proc.WaitForExit(timeoutMs) then
                let output = outputTask.Result
                // It started, it exited, and it said nothing. That is the COMMONEST way this goes
                // wrong - a TablebaseDirectory that exists but holds no table for this piece
                // count, so the directory check upstream passes and the probe still cannot answer
                // - and it produced no error of any kind before.
                if String.IsNullOrWhiteSpace output then
                    reportProberUnusable exePath
                        (sprintf "the prober ran (exit code %d) but returned nothing. Check that %s holds tables for this piece count."
                            proc.ExitCode tablebasePath)
                    None
                else Some output
            else
                try proc.Kill(true) with _ -> ()
                reportProbeTimeout timeoutMs
                None
    with ex ->
        reportProberUnusable exePath ex.Message
        None
