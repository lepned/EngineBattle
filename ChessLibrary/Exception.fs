namespace ChessLibrary

open System
open Microsoft.Extensions.Logging
open System.Globalization
open System.Collections.Generic
open System.Threading.Tasks

module CustomException =

    [<Struct>]
    type FailureKind =
        | Timeout
        | Disconnect
        | Hang
        | IllegalMove
        | ProcessCrash
        | Communication
        | Startup
        | AppShuttingDown

    [<Serializable>]
    type EngineTimeoutException(timeoutMs: int, wasThinking: bool) =
        inherit Exception(sprintf "Engine timed out after %d ms (thinking=%b)" timeoutMs wasThinking)
        member _.TimeoutMs   = timeoutMs
        member _.WasThinking = wasThinking
        member _.Kind = FailureKind.Timeout

    [<Serializable>]
    type EngineDisconnectException(exitCode: int option, stderr: string list) =
        inherit Exception(
            match exitCode with
            | Some c -> sprintf "Engine disconnected (exit=%d). stderr lines=%d" c stderr.Length
            | None   -> sprintf "Engine disconnected (forced kill). stderr lines=%d" stderr.Length
        )
        member _.ExitCode = exitCode
        member _.StdErr   = stderr
        member _.Kind = FailureKind.Disconnect

    [<Serializable>]
    type EngineHangException(silentDurationMs: int, lastOutput: string option) =
        inherit Exception(sprintf "Engine hang (no output for %d ms)" silentDurationMs)
        member _.SilentDurationMs = silentDurationMs
        member _.LastOutput       = lastOutput
        member _.Kind = FailureKind.Hang

    [<Serializable>]
    type IllegalMoveException(attemptedMove: string, positionFen: string) =
        inherit Exception(sprintf "Illegal move '%s' in position." attemptedMove)
        member _.AttemptedMove = attemptedMove
        member _.PositionFen   = positionFen
        member _.Kind = FailureKind.IllegalMove

    [<Serializable>]
    type EngineProcessCrashException(diagnostics: string) =
        inherit Exception("Engine process crashed.")
        member _.Diagnostics = diagnostics
        member _.Kind = FailureKind.ProcessCrash

    [<Serializable>]
    type EngineCommunicationException(message: string) =
        inherit Exception(message)
        member _.Kind = FailureKind.Communication

    [<Serializable>]
    type EngineStartupException(message: string) =
        inherit Exception(message)
        member _.Kind = FailureKind.Startup

    /// Derive from OperationCanceledException so upstream can short-circuit cleanly
    [<Serializable>]
    type AppShuttingDownException(message: string) =
        inherit OperationCanceledException(message)
        member _.Kind = FailureKind.AppShuttingDown


    type CatchContext = {
        EngineName   : string
        OpponentName : string
        GameNumber   : int
        MoveNumber   : int
        TimeControl  : string
        TimeRemaining: TimeSpan option   // cheaper than TimeOnly here; null-safe
        PositionFen  : string
        LastCommand  : string option
        TimestampUtc : DateTime
        MoveHistory  : string
    }

    [<RequireQualifiedAccess>]
    module EngineFailures =

        let private severity (kind: FailureKind) =
            match kind with
            | FailureKind.ProcessCrash
            | FailureKind.Disconnect
            | FailureKind.Startup
            | FailureKind.AppShuttingDown -> LogLevel.Critical
            | FailureKind.Timeout
            | FailureKind.Hang
            | FailureKind.IllegalMove
            | FailureKind.Communication   -> LogLevel.Error

        /// Make a compact human-readable incident line (in addition to structured props).
        let private formatIncident (kind: FailureKind) (ex: exn) (ctx: CatchContext) =
            let tr = 
                match ctx.TimeRemaining with
                | Some ts -> sprintf "%0.3fs" ts.TotalSeconds
                | None    -> "N/A (node-limited)"
            let lc =
                match ctx.LastCommand with
                | Some c -> c
                | None   -> "(none)"
            let ts = ctx.TimestampUtc.ToString("yyyy-MM-dd HH:mm:ss.fff", CultureInfo.InvariantCulture)
            // Keep concise (ops tools prefer short lines); details are captured in structured fields.
            $"[{ts}Z] {kind} | " +
            $"Engine={ctx.EngineName} vs {ctx.OpponentName} | G#{ctx.GameNumber} M#{ctx.MoveNumber} | " +
            $"TC={ctx.TimeControl} TR={tr} | Pos={ctx.PositionFen} | Hist={ctx.MoveHistory} | LastCmd={lc} | Ex={ex.Message}"

        /// Structured log with per-exception enrichment
        let log (logger: ILogger) (ex: exn) (ctx: CatchContext) =
            // Pull failure kind + enrich structured state
            let (kind, enrich: (string * obj) list) =
                match ex with
                | :? EngineTimeoutException as e ->
                    FailureKind.Timeout,
                    [ "TimeoutMs", box e.TimeoutMs
                      "WasThinking", box e.WasThinking ]
                | :? EngineDisconnectException as e ->
                    FailureKind.Disconnect,
                    [ "ExitCode", box (e.ExitCode |> Option.toNullable)
                      "StdErrLines", box e.StdErr
                      "StdErrCount", box e.StdErr.Length ]
                | :? EngineHangException as e ->
                    FailureKind.Hang,
                    [ "SilentDurationMs", box e.SilentDurationMs
                      "LastOutput", box (defaultArg e.LastOutput "(none)") ]
                | :? IllegalMoveException as e ->
                    FailureKind.IllegalMove,
                    [ "AttemptedMove", box e.AttemptedMove
                      "PositionFen", box e.PositionFen ]
                | :? EngineProcessCrashException as e ->
                    FailureKind.ProcessCrash,
                    [ "Diagnostics", box e.Diagnostics ]
                | :? EngineCommunicationException ->
                    FailureKind.Communication, []
                | :? EngineStartupException ->
                    FailureKind.Startup, []
                | :? AppShuttingDownException ->
                    FailureKind.AppShuttingDown, []
                | _ ->
                    // Unknown exception (still log, but label as Communication)
                    FailureKind.Communication,
                    [ "UnknownExceptionType", box (ex.GetType().FullName) ]

            let lvl = severity kind
            let incident = formatIncident kind ex ctx

            // Base structured props (cheap to produce, helpful in logs/queries)
            let baseProps : (string * obj) list =
                [ "FailureKind", box (kind.ToString())
                  "Engine",     box ctx.EngineName
                  "Opponent",   box ctx.OpponentName
                  "GameNumber", box ctx.GameNumber
                  "MoveNumber", box ctx.MoveNumber
                  "TimeControl", box ctx.TimeControl
                  "TimeRemainingSec", box (ctx.TimeRemaining |> Option.map (fun t -> t.TotalSeconds) |> Option.defaultValue Double.NaN)
                  "PositionFen", box ctx.PositionFen
                  "LastCommand", box (defaultArg ctx.LastCommand "(none)")
                  "TimestampUtc", box ctx.TimestampUtc
                  "MoveHistory", box ctx.MoveHistory ]

            let allProps = baseProps @ enrich

            // Emit with structured logging (works with Serilog/sinks, M.E.Logging supports {Property} placeholders)
            let state =
                let kvps = allProps |> Seq.map (fun (k,v) -> KeyValuePair<string,obj>(k,v))
                new Dictionary<string,obj>(kvps)


            logger.Log(lvl, 0, state, ex, fun _ _ -> incident)


/// Result-oriented error model for engine failures.
/// Provides a discriminated union counterpart to the exception types in CustomException,
/// helpers to convert between exceptions and failures, and common combinators/CE builder.
module EngineResult =

    /// Discriminated error form mirroring the exception-based model in CustomException.
    type EngineFailure =
        | Timeout of timeoutMs:int * wasThinking:bool
        | Disconnect of exitCode:int option * stderr:string list
        | Hang of silentDurationMs:int * lastOutput:string option
        | IllegalMove of attemptedMove:string * positionFen:string
        | ProcessCrash of diagnostics:string
        | Communication of message:string
        | Startup of message:string
        | AppShuttingDown of message:string
        | ExceptionThrown of message:string * Exn:exn  // fallback for unknown exceptions

    /// Alias for results returning an EngineFailure on error.
    type Result<'T> = Result<'T, EngineFailure>

    /// Map internal failure to the existing FailureKind union (keeps parity with logging code).
    let failureKind (f: EngineFailure) =
        match f with
        | Timeout _ -> CustomException.FailureKind.Timeout
        | Disconnect _ -> CustomException.FailureKind.Disconnect
        | Hang _ -> CustomException.FailureKind.Hang
        | IllegalMove _ -> CustomException.FailureKind.IllegalMove
        | ProcessCrash _ -> CustomException.FailureKind.ProcessCrash
        | Communication _ -> CustomException.FailureKind.Communication
        | Startup _ -> CustomException.FailureKind.Startup
        | AppShuttingDown _ -> CustomException.FailureKind.AppShuttingDown
        | ExceptionThrown _ -> CustomException.FailureKind.Communication

    /// Convert an EngineFailure into the corresponding exception type defined in CustomException.
    let toException (f: EngineFailure) : exn =
        match f with
        | Timeout (ms, thinking) -> CustomException.EngineTimeoutException(ms, thinking) :> exn
        | Disconnect (code, stderr) -> CustomException.EngineDisconnectException(code, stderr) :> exn
        | Hang (dur, last) -> CustomException.EngineHangException(dur, last) :> exn
        | IllegalMove (mv, fen) -> CustomException.IllegalMoveException(mv, fen) :> exn
        | ProcessCrash diag -> CustomException.EngineProcessCrashException(diag) :> exn
        | Communication msg -> CustomException.EngineCommunicationException(msg) :> exn
        | Startup msg -> CustomException.EngineStartupException(msg) :> exn
        | AppShuttingDown msg -> CustomException.AppShuttingDownException(msg) :> exn
        | ExceptionThrown (msg, ex) -> InvalidOperationException(msg, ex) :> exn

    /// Try to convert an exception into a discriminated EngineFailure.
    /// Unknown exceptions are turned into Communication with the exception message.
    let ofException (ex: exn) : EngineFailure =
        match ex with
        | :? CustomException.EngineTimeoutException as e ->
            Timeout(e.TimeoutMs, e.WasThinking)
        | :? CustomException.EngineDisconnectException as e ->
            Disconnect(e.ExitCode, e.StdErr)
        | :? CustomException.EngineHangException as e ->
            Hang(e.SilentDurationMs, e.LastOutput)
        | :? CustomException.IllegalMoveException as e ->
            IllegalMove(e.AttemptedMove, e.PositionFen)
        | :? CustomException.EngineProcessCrashException as e ->
            ProcessCrash(e.Diagnostics)
        | :? CustomException.EngineCommunicationException as e ->
            Communication(e.Message)
        | :? CustomException.EngineStartupException as e ->
            Startup(e.Message)
        | :? CustomException.AppShuttingDownException as e ->
            AppShuttingDown(e.Message)
        | _ ->
            // Unknown exception -> treat as communication-level failure
            Communication(ex.Message)

    /// Logging helper: logs the failure using existing structured logger code.
    /// Uses CustomException.EngineFailures.log by converting EngineFailure back to an Exception.
    let logFailure (logger: ILogger) (failure: EngineFailure) (ctx: CustomException.CatchContext) =
        let ex = toException failure
        CustomException.EngineFailures.log logger ex ctx

    // Basic combinators -----------------------------------------------------

    let ok (v: 'T) : Result<'T> = Ok v
    let error (e: EngineFailure) : Result<'T> = Error e

    let map (f: 'T -> 'U) (r: Result<'T>) : Result<'U> =
        match r with
        | Ok v -> Ok (f v)
        | Error e -> Error e

    let mapError (f: EngineFailure -> EngineFailure) (r: Result<'T>) : Result<'T> =
        match r with
        | Ok v -> Ok v
        | Error e -> Error (f e)

    let bind (f: 'T -> Result<'U>) (r: Result<'T>) : Result<'U> =
        match r with
        | Ok v -> f v
        | Error e -> Error e

    let ofChoice (choice: Choice<'T, exn>) : Result<'T> =
        match choice with
        | Choice1Of2 v -> Ok v
        | Choice2Of2 ex -> Error (ofException ex)

    /// Run a synchronous function, capturing exceptions into EngineFailure.
    let tryCapture (thunk: unit -> 'T) : Result<'T> =
        try Ok (thunk ())
        with ex -> Error (ofException ex)

    /// Run an async computation and capture exceptions into EngineFailure.
    let tryCaptureAsync (comp: Async<'T>) : Async<Result<'T>> =
        async {
            try
                let! v = comp
                return Ok v
            with ex ->
                return Error (ofException ex)
        }

     /// Run a Task and capture exceptions into EngineFailure.
    let tryCaptureTask (task: Task<'T>) : Task<Result<'T>> =
        task.ContinueWith(fun (t: Task<'T>) ->
            if t.IsFaulted then
                // t.Exception is an AggregateException; take the first inner if present
                let ex =
                    match t.Exception with
                    | null ->
                        // Defensive fallback: try to observe the task result to surface any exception,
                        // otherwise synthesize a descriptive exception.
                        try
                            t.GetAwaiter().GetResult() |> ignore
                            InvalidOperationException("Task faulted but Exception was null.") :> exn
                        with e -> e
                    | agg ->
                        // agg is an AggregateException; prefer the first inner exception when present
                        agg.Flatten().InnerExceptions |> Seq.tryHead |> Option.defaultValue (agg :> exn)
                Error (ofException ex)
            else if t.IsCanceled then
                Error (AppShuttingDown "Task canceled")
            else
                Ok t.Result
        )


    // Computation Expression for convenient workflow usage ------------------

    type EngineResultBuilder() =
        member _.Return(x) = Ok x
        member _.ReturnFrom(r: Result<'T>) = r
        member _.Bind(r, f) = bind f r
        member _.Zero() = Ok ()
        member _.Delay(f: unit -> Result<'T>) = f()

    /// Instance used as `engineResult { ... }`
    let engineResult = EngineResultBuilder()

    // Convenience constructors ----------------------------------------------

    let timeout ms thinking = Timeout(ms, thinking)
    let disconnect code stderr = Disconnect(code, stderr)
    let hang dur last = Hang(dur, last)
    let illegalMove mv fen = IllegalMove(mv, fen)
    let processCrash diag = ProcessCrash(diag)
    let communication msg = Communication(msg)
    let startup msg = Startup(msg)
    let appShuttingDown msg = AppShuttingDown(msg)