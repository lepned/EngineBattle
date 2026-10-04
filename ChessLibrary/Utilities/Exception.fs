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
        TimeRemaining: TimeSpan option   // option rather than a sentinel: absent is not zero
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
            $"\n[{ts}Z] {kind} | " +
            $"Engine={ctx.EngineName} vs {ctx.OpponentName} | G#{ctx.GameNumber} M#{ctx.MoveNumber} | " +
            $"TC={ctx.TimeControl} TR={tr} | Pos={ctx.PositionFen} | Hist={ctx.MoveHistory} | LastCmd={lc} | Ex={ex.Message}\n"

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
