/// gui: the WebGUI of a source checkout, started with `dotnet run`, the browser opened on a page.
module Commands.Gui

open System
open System.Diagnostics
open System.IO
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.App

let mutable private server: Process option = None
let mutable private stopping = false

/// The WebGUI project: found from the Console folder or its build output in a source checkout.
/// None in a published package, which has no project to run (the WebGUI is EngineBattle there).
let tryWebGuiProject () =
    let current = DirectoryInfo(Environment.CurrentDirectory)
    let root =
        if current.FullName.EndsWith "Console" then current.Parent
        else
            // bin/Release/netX.0 layout: four levels up to the solution root
            let mutable dir = current
            for _ in 1..4 do
                if not (isNull dir) && not (isNull dir.Parent) then dir <- dir.Parent
            dir
    if isNull root then None
    else
        let path = Path.Combine(root.FullName, "WebGUI")
        if File.Exists(Path.Combine(path, "WebGUI.csproj")) then Some path else None

/// Starts `dotnet run` in the project; the browser opens on `page` once the server listens.
let private start (project: string) (page: string) (port: int option) =
    let psi =
        ProcessStartInfo(
            FileName = "dotnet",
            Arguments = (match port with Some p -> $"run --urls http://localhost:{p}" | None -> "run"),
            WorkingDirectory = project,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true)
    let proc = new Process(StartInfo = psi, EnableRaisingEvents = true)
    proc.OutputDataReceived.Add(fun a ->
        if not (isNull a.Data) then
            Console.WriteLine a.Data
            if a.Data.Contains "Now listening on:" then
                let url = a.Data.Split("Now listening on:").[1].Trim().TrimEnd('/') + "/" + page.TrimStart('/')
                info $"Opening {url}"
                try Process.Start(ProcessStartInfo(url, UseShellExecute = true)) |> ignore
                with ex -> warn $"cannot open the browser: {ex.Message}")
    proc.ErrorDataReceived.Add(fun a -> if not (isNull a.Data) then Console.Error.WriteLine a.Data)
    // a server that dies on its own (a build error, the port taken) says so
    proc.Exited.Add(fun _ -> if not stopping then error "the WebGUI has exited; press Enter")
    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    server <- Some proc

/// Stops the server and everything it started (dotnet run and the WebGUI under it).
let stop () =
    match server with
    | Some proc when not proc.HasExited ->
        stopping <- true
        info "Stopping the WebGUI..."
        (try proc.Kill true with _ -> ())
        proc.Dispose()
    | _ -> ()
    server <- None

let spec =
    { command [ "gui" ] "Other" "Start the WebGUI of a source checkout" with
        Description =
          Some "Runs the WebGUI project with `dotnet run` and opens the browser on the page. Enter or Ctrl+C stops it. A download has no project to run: start EngineBattle instead."
        Args = [ optionalArg "page" "the page to open (default tournament)" ]
        Options = [ value "port" "N" "Listen on this port (default the WebGUI's own, 5018)" ]
        Examples =
          [ "eb-cli gui", "The tournament page"
            "eb-cli gui analysis/single --port 5100", "Single-engine analysis on port 5100" ] }

let run (p: Parsed) =
    let page = defaultArg (argOf p "page") "tournament"
    if page |> Seq.forall Char.IsDigit then raise (Usage $"'{page}' is a page name; a port is --port {page}")
    let port =
        intOf p "port"
        |> Option.map (fun n -> if n > 0 && n < 65536 then n else raise (Usage $"--port must be 1-65535, got {n}"))
    match tryWebGuiProject () with
    | None ->
        error "there is no WebGUI project here to run (a download has EngineBattle next to eb-cli instead)"
        ExitFailed
    | Some project ->
        info $"""Starting the WebGUI{(match port with Some n -> $" on port {n}" | None -> "")}; Enter or Ctrl+C stops it"""
        Console.CancelKeyPress.Add(fun e ->
            e.Cancel <- true
            stop ()
            exit ExitInterrupted)
        start project page port
        Console.ReadLine() |> ignore
        let died = server |> Option.exists (fun proc -> proc.HasExited)
        stop ()
        if died then ExitFailed else ExitOk

let entries: Entry list = [ { Spec = spec; Run = run } ]
