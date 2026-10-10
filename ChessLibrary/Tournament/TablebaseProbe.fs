module ChessLibrary.TablebaseProbe

open System
open System.IO
open System.Threading
open EngineBattle.Tablebases

/// The folders of a tablebase setting: several are separated by the platform's path separator
/// (';' on Windows, ':' elsewhere), as Fathom expects them.
let tablebaseFolders (tablebasePath: string) =
    if String.IsNullOrWhiteSpace tablebasePath then [||]
    else tablebasePath.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

/// A setting that names at least one folder, every one of which exists.
let tablebasesExist (tablebasePath: string) =
    let folders = tablebaseFolders tablebasePath
    folders.Length > 0 && folders |> Array.forall Directory.Exists

/// Most pieces of any Syzygy table in the folders, from the file names (KBBBBvK.rtbw = 6); 0 when
/// they hold none.
let largestTable (tablebasePath: string) =
    tablebaseFolders tablebasePath
    |> Array.map (fun folder ->
        try
            Directory.EnumerateFiles(folder, "*.rtbw")
            |> Seq.map (fun f -> (Path.GetFileNameWithoutExtension f).Replace("v", "").Length)
            |> Seq.fold max 0
        with _ -> 0)
    |> Array.fold max 0

/// <summary>
/// The tables are probed in this process (Syzygy, a C# port of Fathom), which holds one set of
/// tables: every folder a run has named, in the order they came. The WebGUI runs many tournaments
/// in one process, and two may name different folders; giving the prober all of them means neither
/// takes the other's tables away. A folder's tables are the same data whoever asks.
///
/// Loading again unmaps nothing (a probe on another thread may still be reading the old tables), so
/// it happens only when something changed: a new folder, or tables added to or removed from one.
/// </summary>
let private gate = obj ()
let mutable private loaded: string list = []
let mutable private loadedSignature = ""

let private sameFolder (a: string) (b: string) =
    let trim (p: string) = p.TrimEnd('/', '\\')
    String.Equals(trim a, trim b,
                  if OperatingSystem.IsWindows() || OperatingSystem.IsMacOS() then StringComparison.OrdinalIgnoreCase
                  else StringComparison.Ordinal)

/// The folders and how many table files each holds: a change means the tables must be read again.
let private signature (folders: string list) =
    folders
    |> List.map (fun folder ->
        let count = try Directory.EnumerateFiles(folder, "*.rtb?") |> Seq.length with _ -> -1
        $"{folder}|{count}")
    |> String.concat "\n"

let private load (folders: string list) =
    Syzygy.Init(String.Join(string Path.PathSeparator, folders)) |> ignore
    loaded <- folders
    loadedSignature <- signature folders

/// Adds the setting's folders to the prober's; with `recheck`, also reads them all again when their
/// table files have changed since they were loaded (once per run, not per probe).
let private ensureLoaded (recheck: bool) (tablebasePath: string) =
    lock gate (fun () ->
        let added =
            tablebaseFolders tablebasePath
            |> Array.filter (fun f -> not (loaded |> List.exists (sameFolder f)))
            |> List.ofArray
        if not added.IsEmpty then load (loaded @ added)
        elif recheck && signature loaded <> loadedSignature then load loaded)

/// Set when a probe found no table for its position; cleared by each run, so the WebGUI reports
/// again for the next tournament.
let private noAnswerReported = ref 0

let resetProbeReports () =
    noAnswerReported.Value <- 0

/// <summary>
/// What the tablebases say about a position, from the side to move's view with the 50-move rule
/// counted (CursedWin and BlessedLoss are wins and losses the rule turns into draws), or None: more
/// pieces than the tables hold, or no table for this material.
/// </summary>
let probe (tablebasePath: string) (fen: string) (pieces: int) : TbWdl option =
    ensureLoaded false tablebasePath
    if pieces > Syzygy.MaxPieces then None
    else
        let result = Syzygy.ProbeRoot fen
        if result.Ok then Some result.Wdl
        else
            if Interlocked.Exchange(noAnswerReported, 1) = 0 then
                Console.Error.WriteLine(
                    sprintf "No tablebase answer for %s (a table missing from %s?); such positions are played on. Later ones are not repeated." fen tablebasePath)
            None

/// A new run: reports cleared and, with tablebase adjudication on, the tables found before any game
/// needs them (read again if they have changed since the last run).
let startRun (useTablebases: bool) (men: int) (tablebasePath: string) =
    resetProbeReports ()
    if useTablebases && tablebasesExist tablebasePath then
        ensureLoaded true tablebasePath
        let largest = largestTable tablebasePath
        if largest < men then
            Console.Error.WriteLine(
                sprintf "Tablebases in %s go up to %d pieces; positions with more are not adjudicated from tablebases." tablebasePath largest)
