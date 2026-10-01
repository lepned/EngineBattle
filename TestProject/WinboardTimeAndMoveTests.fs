/// What a Winboard engine is told about time, and which of its moves are taken (WinboardProtocol):
/// the three time commands that cost games in the engine survey (otim 0, st 0, a rounded-up
/// increment) and the stale move left over from an earlier game.
module WinboardTimeAndMoveTests

open System
open Microsoft.Extensions.Logging
open Xunit
open ChessLibrary.WinboardProtocol
open ChessLibrary.TypesDef.CoreTypes

let private logger =
    { new ILogger with
        member _.BeginScope(_) = { new IDisposable with member _.Dispose() = () }
        member _.IsEnabled(_) = false
        member _.Log(_, _, _, _, _) = () }

let private v2 (config: WinboardConfig) =
    let handler = WinboardHandler(logger, "Test", config)
    handler.ProcessFeatureLine("feature done=1") |> ignore
    handler.UciToWinboard("position startpos") |> ignore
    handler

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

// ---- otim ----

[<Fact>]
let ``an opponent with no clock is not sent as otim 0`` () =
    // a node-limited or time-per-move opponent arrives as 0 ms; otim 0 made engines play instantly
    let handler = v2 { WinboardConfig.Default with TimeControlStrategy = TimeOtimOnly }
    let white = handler.GoToWinboard "go wtime 30000 btime 0" true false
    Assert.Contains("time 3000", white)
    Assert.Contains("otim 3000", white)
    let black = handler.GoToWinboard "go wtime 0 btime 25000" false false
    Assert.Contains("time 2500", black)
    Assert.Contains("otim 2500", black)

[<Fact>]
let ``an opponent with a clock is sent as it is`` () =
    let handler = v2 { WinboardConfig.Default with TimeControlStrategy = TimeOtimOnly }
    let cmds = handler.GoToWinboard "go wtime 30000 btime 12000" true false
    Assert.Contains("time 3000", cmds)
    Assert.Contains("otim 1200", cmds)

// ---- level increment ----

let private levelOf (config: WinboardConfig) (incMs: int) =
    let handler = v2 config
    handler.GoToWinboard (sprintf "go wtime 10000 btime 10000 winc %d binc %d" incMs incMs) true false
    |> List.find (fun c -> c.StartsWith "level")

[<Fact>]
let ``the level increment is rounded down, never up`` () =
    Assert.Equal("level 0 0:10 0", levelOf WinboardConfig.Default 100)
    Assert.Equal("level 0 0:10 0", levelOf WinboardConfig.Default 500)   // was 1: more than the engine gets
    Assert.Equal("level 0 0:10 2", levelOf WinboardConfig.Default 2000)

[<Fact>]
let ``MinLevelIncrement raises a small increment for engines that need one`` () =
    let jonny = { WinboardConfig.Default with MinLevelIncrement = 1 }
    Assert.Equal("level 0 0:10 1", levelOf jonny 100)
    Assert.Equal("level 0 0:10 2", levelOf jonny 2000)
    // no increment at all is not turned into one the engine never gets
    Assert.Equal("level 0 0:10 0", levelOf jonny 0)

// ---- st ----

let private stOf (go: string) =
    let handler = v2 { WinboardConfig.Default with TimeControlStrategy = StWithTime }
    handler.GoToWinboard go true false |> List.find (fun c -> c.StartsWith "st ")

[<Fact>]
let ``st counts the increment and rounds down`` () =
    // 30+0.5: 750 ms of clock + 500 ms increment a move - it was st 0, a depth-1 search
    Assert.Equal("st 1", stOf "go wtime 30000 btime 30000 winc 500 binc 500")
    // 60+1: 1.5 s + 1 s, rounded down
    Assert.Equal("st 2", stOf "go wtime 60000 btime 60000 winc 1000 binc 1000")
    // 10+0.1: 0.35 s a move, under a second - the engine's fastest move
    Assert.Equal("st 0", stOf "go wtime 10000 btime 10000 winc 100 binc 100")

[<Fact>]
let ``st is never more than half the clock, whatever the increment`` () =
    // the increment comes after the move: at 1+1 a second left gave st 1, a loss on time
    Assert.Equal("st 0", stOf "go wtime 1000 btime 1000 winc 1000 binc 1000")
    // 3+1 with 1.9 s left: st 1 kept being sent while the overhead drained the clock
    Assert.Equal("st 0", stOf "go wtime 1900 btime 1900 winc 1000 binc 1000")
    Assert.Equal("st 1", stOf "go wtime 3000 btime 3000 winc 1000 binc 1000")

// ---- stale moves ----

let private blackToMove () =
    let handler = WinboardHandler(logger, "Test", WinboardConfig.Default)
    handler.ProcessFeatureLine("feature done=1") |> ignore
    handler.UciToWinboard(sprintf "position fen %s moves e2e4" startFen) |> ignore
    handler

[<Fact>]
let ``a move that is not legal in the engine's position is dropped`` () =
    // e2e4 again, as Black: what Comet's last move of the previous game looked like here
    Assert.Equal(None, (blackToMove ()).ProcessOutput "move e2e4")
    Assert.Equal(None, (blackToMove ()).ProcessOutput "1. ... e2e4")

[<Fact>]
let ``a legal move is taken`` () =
    Assert.Equal(Some "bestmove e7e5", (blackToMove ()).ProcessOutput "move e7e5")
    Assert.Equal(Some "bestmove e7e5", (blackToMove ()).ProcessOutput "1. ... e7e5")

// ---- dummy level ----

[<Fact>]
let ``the dummy level for thinking output is sudden death`` () =
    // level 40 5 0 made Comet budget for a clock back at move 40: 1.5-3 s a move with 3-12 s left
    let handler = WinboardHandler(logger, "Test", { WinboardConfig.Default with RequiresLevelForThinkingOutput = true })
    Assert.Contains("level 0 5 0", handler.GetPostInitCommands())

// ---- pause between commands ----

[<Fact>]
let ``CommandDelayMs pauses before every line of a burst but the first, and go keeps the longer pre-go delay`` () =
    // TheTurk crashed at about 3 in 100 game starts on force, setboard, st, time, otim sent at once
    let wb config = ChessLibrary.EngineWire.Winboard(WinboardHandler(logger, "Test", config))
    let engine config = { EngineConfig.Empty with WinboardConfig = Some config }
    let turk = { WinboardConfig.Default with CommandDelayMs = 20 }
    let delay config i line = ChessLibrary.EngineWire.lineDelayMs (engine config) (wb config) i line
    Assert.Equal(0, delay turk 0 "force")
    Assert.Equal(20, delay turk 1 "setboard 8/8/8/8/8/8/8/K6k w - -")
    Assert.Equal(100, delay turk 5 "go")
    // the default sends a burst at once, as before
    Assert.Equal(0, delay WinboardConfig.Default 1 "setboard 8/8/8/8/8/8/8/K6k w - -")
    Assert.Equal(100, delay WinboardConfig.Default 5 "go")

[<Fact>]
let ``an engine with ping gets go at once, as cutechess sends it`` () =
    let withPing config =
        let handler = WinboardHandler(logger, "Test", config)
        handler.ProcessFeatureLine("feature ping=1 setboard=1 done=1") |> ignore
        ChessLibrary.EngineWire.Winboard handler
    let engine config = { EngineConfig.Empty with WinboardConfig = Some config }
    let delay config = ChessLibrary.EngineWire.lineDelayMs (engine config) (withPing config) 5 "go"
    Assert.Equal(0, delay WinboardConfig.Default)
    // an engine that still wants a pause gets it from CommandDelayMs
    Assert.Equal(20, delay { WinboardConfig.Default with CommandDelayMs = 20 })
