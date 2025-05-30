open System
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Control


// Define a message type for the agent
type OutputMessage =
    | Line of string
    | Stop

// Function to create an agent for reading output
let createOutputAgent (prefix: string) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () = async {
            let! msg = inbox.Receive()
            match msg with
            | Line line -> 
                Console.WriteLine(prefix + line)
                return! loop()
            | Stop -> 
                Console.WriteLine(prefix + "Agent stopped")
                //() // Exit the loop
        }
        loop ())

// Function to run an interactive executable
let runInteractiveExecutable1 (executablePath: string) =
    // Create a new process
    let myProcess = new Process()
    myProcess.StartInfo.FileName <- executablePath

    // Set up process to capture input, output, and error
    myProcess.StartInfo.RedirectStandardInput <- true
    myProcess.StartInfo.RedirectStandardOutput <- true
    myProcess.StartInfo.RedirectStandardError <- true
    myProcess.StartInfo.UseShellExecute <- false
    myProcess.StartInfo.CreateNoWindow <- true

    // Start the process
    myProcess.Start() |> ignore

    // Get the standard input, output, and error streams
    let inputWriter = myProcess.StandardInput
    let outputReader = myProcess.StandardOutput
    let errorReader = myProcess.StandardError

    // Create agents for output and error streams
    let outputAgent = createOutputAgent "Output: "
    let errorAgent = createOutputAgent "Error: "

    // Function to read stream asynchronously and send lines to the agent
    let readStreamAsync (reader: StreamReader) (agent: MailboxProcessor<OutputMessage>) =
        async {
            while not reader.EndOfStream do
                let line = reader.ReadLine()
                agent.Post(Line line)
            agent.Post(Stop)
        }

    // Start reading output and error streams asynchronously
    let outputTask = readStreamAsync outputReader outputAgent |> Async.StartAsTask
    let errorTask = readStreamAsync errorReader errorAgent |> Async.StartAsTask

    // Function to send a command
    let sendCommand (command: string) =
        inputWriter.WriteLine(command)
        inputWriter.Flush()

    // Function to handle user input in FSI
    let rec handleUserInput () =
        printf "Enter command: "
        let command = Console.ReadLine()
        if command <> null && command.ToLower() <> "exit" then
            sendCommand command
            handleUserInput()
        else
            sendCommand "exit"

    // Start handling user input
    handleUserInput()

    // Wait for the process to exit
    myProcess.WaitForExit()

    // Wait for the output reading tasks to complete
    outputTask.Wait()
    errorTask.Wait()

// Example usage: interacting with cmd.exe
//let executablePath = "cmd.exe"
let ceresPath = @"C:/Dev/Chess/Engines/Ceres/v0.97RC3/Ceres.exe"

runInteractiveExecutable1 ceresPath

