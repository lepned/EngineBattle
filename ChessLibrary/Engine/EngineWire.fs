namespace ChessLibrary

open System
open Microsoft.Extensions.Logging
open TypesDef.CoreTypes
open ChessLibrary.WinboardProtocol
open ChessLibrary.WinboardIntegration

/// What travels over an engine's pipes, by protocol. Both engine wrappers speak UCI-shaped
/// commands and read UCI-shaped lines; a Winboard engine gets them translated both ways by its
/// WinboardHandler. Everything that differs between the two protocols is decided here, once,
/// instead of in a `match winboardHandler with` in every member of both wrappers.
module internal EngineWire =

  type Protocol =
    | Uci
    | Winboard of WinboardHandler

  /// UCI unless the engine def says Winboard/xboard.
  let protocolFor (config: EngineConfig) (logger: ILogger option) =
    match createHandlerIfNeeded config logger with
    | Some handler -> Winboard handler
    | None -> Uci

  let isWinboard = function Winboard _ -> true | Uci -> false

  /// The lines to send for one UCI-shaped command. The analysis wrapper asks for analysis mode:
  /// a Winboard engine then gets the full position set up for every search.
  let outbound (protocol: Protocol) (analysisMode: bool) (command: string) : string list =
    match protocol with
    | Uci -> [ command ]
    | Winboard handler -> handler.UciToWinboard(command, analysisMode = analysisMode)

  /// Winboard engines without ping support get time to take in time/otim before "go". Zero for
  /// UCI engines.
  let preGoDelayMs (config: EngineConfig) (protocol: Protocol) (line: string) =
    match protocol with
    | Winboard _ when line.StartsWith("go", StringComparison.Ordinal) ->
        config.WinboardConfig |> Option.map (fun wbc -> wbc.PreGoDelayMs) |> Option.defaultValue 100
    | _ -> 0

  /// A line read from the engine as the tournament wrapper sees it: a Winboard line translated to
  /// UCI where the handler knows how, otherwise the line as it came.
  let inboundOrRaw (protocol: Protocol) (line: string) =
    match protocol with
    | Winboard handler when not (String.IsNullOrWhiteSpace line) ->
        match handler.ProcessOutput line with
        | Some translated -> translated
        | None -> line
    | _ -> line

  /// Whether the engine can be reused for the next game without a restart (CECP reuse=0 cannot).
  let canReuse = function
    | Winboard handler -> handler.CanReuse
    | Uci -> true

  let forceV1 (config: EngineConfig) =
    config.WinboardConfig |> Option.map (fun wbc -> wbc.ForceV1Mode) |> Option.defaultValue false

  /// A shorter form of a long position command for the log: "position with 40 moves".
  let shortForLog (command: string) =
    if command.StartsWith("position", StringComparison.Ordinal) && command.Length > 100 then
      let movesIdx = command.IndexOf("moves", StringComparison.Ordinal)
      if movesIdx > 0 then
        let moves = command.Substring(movesIdx + 6).Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        sprintf "position with %d moves" moves.Length
      else "position startpos"
    else command
