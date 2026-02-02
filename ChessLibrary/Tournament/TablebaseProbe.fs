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

/// Runs Fathom with a timeout, returning None on timeout or error
let runFathomSafe (tablebasePath: string) (fen: string) (timeoutMs:int) : string option =
    try
        let exePath = getFathomExecutablePath ()
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
        if not (proc.Start()) then None
        else
            if proc.WaitForExit(timeoutMs) then
                let out = proc.StandardOutput.ReadToEnd()
                Some out
            else
                try proc.Kill(true) with _ -> ()
                None
    with _ -> None
