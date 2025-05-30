module ChessLibrary.ConsoleHandler

open System
open System.Runtime.InteropServices
open System.Threading

// Define an enumeration for the console control events
type ConsoleCtrlEvent =
    | CTRL_C_EVENT = 0
    | CTRL_BREAK_EVENT = 1
    | CTRL_CLOSE_EVENT = 2
    | CTRL_LOGOFF_EVENT = 5
    | CTRL_SHUTDOWN_EVENT = 6

// Define a custom handler function that takes an integer parameter and returns a boolean value
//let CustomHandler (cts: CancellationTokenSource) (sign: int) =
//    // Convert the integer parameter to a ConsoleCtrlEvent value using pattern matching
//    let event = match sign with
//                | 0 -> ConsoleCtrlEvent.CTRL_C_EVENT
//                | 1 -> ConsoleCtrlEvent.CTRL_BREAK_EVENT
//                | 2 -> ConsoleCtrlEvent.CTRL_CLOSE_EVENT
//                | 5 -> ConsoleCtrlEvent.CTRL_LOGOFF_EVENT
//                | 6 -> ConsoleCtrlEvent.CTRL_SHUTDOWN_EVENT

//    // Perform some action based on the event type using pattern matching
//    match event with
//    | ConsoleCtrlEvent.CTRL_C_EVENT ->
//        printfn "Ctrl + C pressed"
//        true // Return true to enable default handler for Ctrl + C 
//    | ConsoleCtrlEvent.CTRL_BREAK_EVENT ->
//        printfn "Ctrl + Break pressed"
//        cts.Cancel()
//        false // Return false to disable default handler for Ctrl + Break 
//    | ConsoleCtrlEvent.CTRL_CLOSE_EVENT ->
//        printfn "Console window closing"
//        true // Return true to enable default handler for console window closing 
//    | ConsoleCtrlEvent.CTRL_LOGOFF_EVENT ->
//        printfn "User logging off"
//        true // Return true to enable default handler for user logging off 
//    | ConsoleCtrlEvent.CTRL_SHUTDOWN_EVENT ->
//        printfn "System shutting down"
//        true // Return true to enable default handler for system shutting down 



//// Declare a delegate type for the handler function
//type ConsoleCtrlDelegate = delegate of int -> bool

//// Import the SetConsoleCtrlHandler function from kernel32.dll
//[<DllImport("kernel32.dll")>]
//extern bool SetConsoleCtrlHandler(ConsoleCtrlDelegate HandlerRoutine, bool Add)

//// Define a constant for the CTRL_BREAK_EVENT signal
//let CTRL_BREAK_EVENT = 1
