/// What EngineBattle answers a Winboard engine's features with, and which of its output lines it
/// takes for a move, a resignation or nothing (WinboardProtocol).
module WinboardHandshakeAndOutputTests

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

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let private handlerAt (fen: string) =
    let handler = WinboardHandler(logger, "Test", WinboardConfig.Default)
    handler.ProcessFeatureLine("feature setboard=1 done=1") |> ignore
    handler.TakeFeatureReplies() |> ignore
    handler.UciToWinboard(sprintf "position fen %s" fen) |> ignore
    handler

// ---- feature negotiation ----

[<Fact>]
let ``every feature is answered, san=1 and unknown ones rejected`` () =
    Assert.Equal("accepted ping", featureReply ("ping", "1"))
    Assert.Equal("accepted san", featureReply ("san", "0"))
    // EngineBattle sends coordinate moves; a rejected san tells the engine to expect them
    Assert.Equal("rejected san", featureReply ("san", "1"))
    Assert.Equal("rejected frobnicate", featureReply ("frobnicate", "1"))

[<Fact>]
let ``the replies follow the feature line, and done=0 waits for done=1`` () =
    let handler = WinboardHandler(logger, "Test", WinboardConfig.Default)
    handler.ProcessFeatureLine("feature ping=1 san=1 myname=\"Old Engine 1.0\" done=0") |> ignore
    Assert.Equal<string list>([ "accepted ping"; "rejected san"; "accepted myname"; "accepted done" ], handler.TakeFeatureReplies())
    Assert.True(handler.AwaitingDone)
    Assert.False(handler.IsInitialized)
    Assert.Equal(Some "Old Engine 1.0", handler.Features.MyName)
    Assert.False(handler.Features.San)
    handler.ProcessFeatureLine("feature done=1") |> ignore
    Assert.False(handler.AwaitingDone)
    Assert.True(handler.IsInitialized)
    Assert.Equal<string list>([ "accepted done" ], handler.TakeFeatureReplies())
    Assert.Empty(handler.TakeFeatureReplies())

// ---- moves ----

[<Fact>]
let ``the move formats engines use are taken`` () =
    let at line = (handlerAt startFen).ProcessOutput line
    Assert.Equal(Some "bestmove e2e4", at "move e2e4")
    Assert.Equal(Some "bestmove e2e4", at "1. e2e4")
    Assert.Equal(Some "bestmove e2e4", at "1. ... e2e4")
    Assert.Equal(Some "bestmove e2e4", at "My move is: e2e4")
    Assert.Equal(Some "bestmove e2e4", at "e2e4")
    Assert.Equal(Some "bestmove e2e4", at "e2-e4")
    Assert.Equal(Some "bestmove g1f3", at "move Nf3")
    let castle = (handlerAt "4k3/8/8/8/8/8/8/4K2R w K - 0 1").ProcessOutput
    Assert.Equal(Some "bestmove e1g1", castle "move O-O")
    Assert.Equal(Some "bestmove e1g1", castle "move 0-0")
    Assert.Equal(Some "bestmove e1g1", castle "1. ... 0-0")
    let promote = (handlerAt "7k/P7/8/8/8/8/8/K7 w - - 0 1").ProcessOutput
    Assert.Equal(Some "bestmove a7a8q", promote "move a7a8q")
    Assert.Equal(Some "bestmove a7a8q", promote "move a7a8=q")
    // without the piece: a queen, as in the PV (it was dropped as "not legal" before)
    Assert.Equal(Some "bestmove a7a8q", promote "move a7a8")
    // a rook moving to the last rank is not touched
    Assert.Equal(Some "bestmove a7a8", (handlerAt "7k/R7/8/8/8/8/8/K7 w - - 0 1").ProcessOutput "move a7a8")

[<Fact>]
let ``engine chatter with a move in it is not taken for a move`` () =
    // the first coordinate token anywhere in a line used to be taken
    let at line = (handlerAt startFen).ProcessOutput line
    Assert.Equal(None, at "Hint: e2e4")
    Assert.Equal(None, at "e2e4 is the book move")
    Assert.Equal(None, at "tellothers e2e4")
    Assert.Equal(None, at "1. e4 e5 2. Nf3")
    Assert.Equal(None, at "offer draw")
    Assert.Equal(None, at "0.25")
    Assert.Equal(None, at "12. 345")

// ---- resignation ----

[<Fact>]
let ``resign and a claim of its own loss are a resignation`` () =
    Assert.Equal(Some "bestmove resign", (handlerAt startFen).ProcessOutput "resign")
    // white to move gives the game to black
    Assert.Equal(Some "bestmove resign", (handlerAt startFen).ProcessOutput "0-1 {White resigns}")
    let blackToMove = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
    Assert.Equal(Some "bestmove resign", (handlerAt blackToMove).ProcessOutput "1-0 {Black resigns}")

[<Fact>]
let ``a win or a draw the engine claims is left to EngineBattle`` () =
    Assert.Equal(None, (handlerAt startFen).ProcessOutput "1-0 {White mates}")
    Assert.Equal(None, (handlerAt startFen).ProcessOutput "1/2-1/2 {Draw by repetition}")

// ---- position ----

[<Fact>]
let ``position startpos starts from the start position, not the last one`` () =
    let handler = handlerAt startFen
    handler.UciToWinboard("position startpos moves e2e4") |> ignore
    let cmds = handler.UciToWinboard("position startpos moves e2e4 e7e5")
    // played on top of the last position, e2e4 again was illegal and the board stayed after 1.e4
    let setboard = cmds |> List.find (fun c -> c.StartsWith "setboard")
    Assert.StartsWith("setboard rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w", setboard)

// ---- a rejected command ----

[<Fact>]
let ``a command the engine rejects in a game is logged once, naming the position it was sent`` () =
    let logged = System.Collections.Generic.List<LogLevel * string>()
    let capture =
        { new ILogger with
            member _.BeginScope(_) = { new IDisposable with member _.Dispose() = () }
            member _.IsEnabled(_) = true
            member _.Log(level, _, state, ex, formatter) = logged.Add((level, formatter.Invoke(state, ex))) }
    let handler = WinboardHandler(capture, "Test", WinboardConfig.Default)
    handler.ProcessFeatureLine("feature setboard=1 done=1") |> ignore
    handler.UciToWinboard(sprintf "position fen %s" startFen) |> ignore
    logged.Clear()
    Assert.Equal(None, handler.ProcessOutput "Error (unknown command): 0")
    Assert.Equal(None, handler.ProcessOutput "Error (unknown command): 0")
    let warnings = logged |> Seq.filter (fun (level, _) -> level = LogLevel.Warning) |> Seq.map snd |> List.ofSeq
    Assert.Single(warnings) |> ignore
    Assert.Contains("rejected a command", warnings.Head)
    Assert.Contains("setboard " + startFen, warnings.Head)

// ---- the PV of a thinking line ----

let private pvOf (fen: string) (line: string) =
    match (handlerAt fen).ProcessOutput line with
    | Some info -> info.Substring(info.IndexOf(" pv ") + 4)
    | None -> ""

[<Fact>]
let ``annotated PV moves keep the whole variation`` () =
    // Comet's fail-low mark: "g1f3?" came out as f2f3, and "f1b5!" ended the PV
    Assert.Equal("g1f3 d7d5 d2d4", pvOf startFen "9 25 23 598019 g1f3? d7d5 d2d4")
    Assert.Equal("g1f3 d7d5 d2d4", pvOf startFen "9 25 23 598019 g1f3! d7d5 d2d4")
    // TheTurk castles with zeros, which lost the hyphen and ended the PV
    Assert.Equal("e1g1 e8d7", pvOf "4k3/8/8/8/8/8/8/4K2R w K - 0 1" "5 300 10 5000 0-0 Kd7")
    // Crafty's hash-table mark and check signs
    Assert.Equal("e2e4 e7e5 d1h5", pvOf startFen "5 20 10 5000 1. e4 e5 2. Qh5+ <HT>")
    // a promotion written without the piece is a queen
    Assert.Equal("a7a8q h8h7", pvOf "7k/P7/8/8/8/8/8/K7 w - - 0 1" "5 900 10 5000 a7a8 h8h7")
