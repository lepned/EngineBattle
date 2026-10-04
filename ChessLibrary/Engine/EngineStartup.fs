/// Start-up pieces both engine classes share: arguments, option checks, network name, defaults.
module ChessLibrary.EngineStartup

open System
open System.Collections.Generic
open System.IO
open ChessLibrary.TypesDef.CoreTypes

/// Lc0 lists its hidden options only with --show-hidden.
let arguments (config: EngineConfig) (isLc0: bool) =
  if not (String.IsNullOrEmpty config.Args) then
    if isLc0 && not (config.Args.Contains "--show-hidden") then config.Args + " --show-hidden"
    else config.Args
  elif isLc0 then "--show-hidden"
  else ""

/// Ceres takes its network in Args, after a colon.
let ceresNetwork (config: EngineConfig) =
  if String.IsNullOrEmpty config.Args then ""
  else
    let parts = config.Args.Split ':'
    if parts.Length > 1 then parts.[1] else ""

/// The network a setoption names (WeightsFile, Network, EvalFile), if it names one.
let networkIn (command: string) =
  let lower = command.ToLower()
  if lower.Contains "weights" || lower.Contains "network" || lower.Contains "evalfile" then
    let parts = command.Split ' '
    // QUIRK (pinned): only the last extension goes, so x.pb.gz is called x.pb
    let n = Path.GetFileNameWithoutExtension parts.[parts.Length - 1]
    if String.IsNullOrEmpty n then None else Some n
  else None

type OptionCheck =
  /// With the engine's default when the value differs from it.
  | Valid of name: string * value: string * changedFrom: string option
  | Invalid of name: string * value: string
  | Malformed

/// A setoption command checked against the options the engine listed.
let check (options: Dictionary<string, UciOption.UciOption>) (command: string) =
  match UciOption.parseSetOptionCommand command with
  | Some (name, value) when UciOption.validateSetOption options (name, value) ->
      Valid (name, value, UciOption.getNoneDefaultSetOption options (name, value) |> Option.map (fun (_, def, _) -> def))
  | Some (name, value) -> Invalid (name, value)
  | None -> Malformed

/// The setoption command with the option name in the engine's own spelling.
let inEngineSpelling (options: Dictionary<string, UciOption.UciOption>) (command: string) =
  match UciOption.parseSetOptionCommand command with
  | Some (name, value) ->
      match options.TryGetValue name with
      | true, o -> sprintf "setoption name %s value %s" o.Name value
      | false, _ -> command
  | None -> command

/// The engine's own move-overhead option and `ms` clamped into its range: the name in the
/// engine's spelling ("Move Overhead", "MoveOverheadMs"...); None when it has none or the def sets it.
let moveOverhead (options: Dictionary<string, UciOption.UciOption>) (configured: string seq) (ms: int64) =
  let setByDef (name: string) = configured |> Seq.exists (fun c -> c.Equals(name, StringComparison.OrdinalIgnoreCase))
  options.Values
  |> Seq.tryPick (fun o ->
      match o.OptionType with
      | UciOption.Spin (lo, hi, _) when o.Name.Contains("overhead", StringComparison.OrdinalIgnoreCase) && not (setByDef o.Name) ->
          Some (o.Name, max lo (min hi ms))
      | _ -> None)

/// The engine's default for each option that has a value.
let defaults (options: Dictionary<string, UciOption.UciOption>) =
  [ for opt in options do
      match opt.Value.OptionType with
      | UciOption.Check b -> yield opt.Key, box b
      | UciOption.Spin (_, _, def) -> yield opt.Key, box def
      | UciOption.Combo (_, def) -> yield opt.Key, box def
      | UciOption.String s -> yield opt.Key, box s
      | _ -> () ]
