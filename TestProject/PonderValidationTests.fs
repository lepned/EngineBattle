/// AllowPondering: EngineBattle sets the UCI Ponder option itself (Configuration.Validation
/// .withPonderOption), and nothing in an engine def has to say "Ponder": true any more.
module PonderValidationTests

open System.Collections.Generic
open Xunit
open ChessLibrary.Configuration
open ChessLibrary.TypesDef.CoreTypes

let private engine (name: string) (protocol: string) (options: (string * obj) list) =
    { EngineConfig.Empty with Name = name; Protocol = protocol; Options = Dictionary<string, obj>(dict options) }

let private ponderOf (config: EngineConfig) =
    match config.Options.TryGetValue "Ponder" with
    | true, v -> Some (unbox<bool> v)
    | _ -> None

[<Fact>]
let ``a UCI engine is told Ponder exactly when AllowPondering is on, whatever its def says`` () =
    let plain = engine "SF" "UCI" [ "Hash", box 64 ]
    let saysFalse = engine "Lc0" "UCI" [ "ponder", box false ]
    let saysTrue = engine "Other" "UCI" [ "Ponder", box true ]
    Assert.Equal(Some true, ponderOf (Validation.withPonderOption true plain))
    Assert.Equal(Some true, ponderOf (Validation.withPonderOption true saysFalse))
    Assert.Equal(Some false, ponderOf (Validation.withPonderOption false saysTrue))
    // one Ponder only, whatever its spelling in the def, and the other options kept
    let set = Validation.withPonderOption true saysFalse
    Assert.Equal(1, set.Options.Keys |> Seq.filter (fun k -> k.ToLowerInvariant() = "ponder") |> Seq.length)
    Assert.Equal(box 64, (Validation.withPonderOption true plain).Options.["Hash"])

[<Fact>]
let ``the def itself is not changed, and a Winboard engine gets no Ponder`` () =
    let plain = engine "SF" "UCI" []
    Validation.withPonderOption true plain |> ignore
    Assert.False(plain.Options.ContainsKey "Ponder")
    let winboard = engine "OldEngine" "Winboard" []
    Assert.Same(winboard, Validation.withPonderOption true winboard)

// ---- told Ponder only when it will be asked to ponder ----

open System
open ChessLibrary.TimeControlTypes
open ChessLibrary.TypesDef.Tournament

let private clock = { Id = 1; Fixed = TimeSpan.FromSeconds 10.0; Increment = TimeSpan.FromSeconds 0.1; NodeLimit = false; Nodes = 0; MoveTime = TimeSpan.Zero; MovesToGo = 0 }
let private perMove = { clock with Id = 2; Fixed = TimeSpan.Zero; Increment = TimeSpan.Zero; MoveTime = TimeSpan.FromSeconds 1.0 }

let private tourny allow preventDeviation =
    { Tournament.Empty with
        AllowPondering = allow
        PreventMoveDeviation = preventDeviation
        TimeControl = { TimeConfigs = [ clock; perMove ]; WmovesToGo = 0; BmovesToGo = 0 } }

[<Fact>]
let ``Ponder is true only for an engine that will be asked to ponder`` () =
    let onClock = { engine "SF" "UCI" [] with TimeControlID = 1 }
    let onMoveTime = { engine "Lc0" "UCI" [] with TimeControlID = 2 }
    Assert.True(Validation.willBeAskedToPonder (tourny true false) onClock)
    // a time per move has no clock to ponder against; deviation prevention never ponders
    Assert.False(Validation.willBeAskedToPonder (tourny true false) onMoveTime)
    Assert.False(Validation.willBeAskedToPonder (tourny true true) onClock)
    Assert.False(Validation.willBeAskedToPonder (tourny false false) onClock)
    Assert.Equal(Some true, ponderOf (Validation.ponderOptionFor (tourny true false) onClock))
    Assert.Equal(Some false, ponderOf (Validation.ponderOptionFor (tourny true false) onMoveTime))
    Assert.Equal(Some false, ponderOf (Validation.ponderOptionFor (tourny true true) onClock))
