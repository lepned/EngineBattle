/// book repertoire: the tree rules on a fake engine, and the PGN it is written as.
module OpeningRepertoireTests

open Xunit
open ChessLibrary
open ChessLibrary.MiscTypes
open ChessLibrary.OpeningRepertoire

let private start = startPosition
let private play (moves: string list) = moves |> List.fold (fun f m -> (BoardUtils.tryMakeMove f m).Value) start
let private key (fen: string) = fen.Split(' ') |> Array.truncate 4 |> String.concat " "

/// A judge that knows a few positions: their lines (UCI moves separated by spaces), best first.
let private judge (table: (string list * (string * float) list) list) =
    let known = table |> List.map (fun (moves, lines) -> key (play moves), lines) |> dict
    let search (fen: string) (k: int) =
        match known.TryGetValue(key fen) with
        | true, lines -> lines |> List.truncate k |> List.map (fun (m, e) -> { Pv = m.Split ' ' |> List.ofArray; Eval = CP e; Policy = None })
        | _ -> []
    new SearchAgent("fake", "", None, (fun f _ k -> search f k), ignore)

let private build settings agent book =
    // one line, so the asserts do not depend on where the movetext wraps
    (OpeningRepertoire.build settings agent None None book start [] "test" |> Async.RunSynchronously).Pgn.Replace("\r\n", " ").Replace("\n", " ")

[<Fact>]
let ``replies near the best are explored, the others answered and punished, a catastrophic one left out`` () =
    let agent =
        judge
            [ [], [ "e2e4", 0.3 ]
              [ "e2e4" ], [ "e7e5", 0.2; "c7c5", 0.25; "g8f6", 1.0; "g7g5", 4.0 ]
              [ "e2e4"; "e7e5" ], [ "g1f3", 0.2 ]
              [ "e2e4"; "c7c5" ], [ "g1f3", 0.25 ]
              [ "e2e4"; "f7f6" ], [ "d2d4", 1.5 ]
              [ "e2e4"; "g8f6" ], [ "e4e5", 1.0 ]
              [ "e2e4"; "g7g5" ], [ "d2d4", 4.0 ] ]
    let settings = { Settings.Defaults with White = true; Plies = 2; Replies = 2; Traps = 0 }
    let pgn = build settings agent (fun fen -> if key fen = key (play [ "e2e4" ]) then [ "f7f6", 40.0 ] else [])
    // e5 is the mainline and c5 explored; Nf6 (?!) and the book's f6 (?) get our ! answer; g5 loses 3.8, left out
    Assert.Contains("1. e4 {[%eval 0.30]} 1... e5 {[%eval 0.20]} (1... c5 {[%eval 0.25]} 2. Nf3 {[%eval 0.25]}) (1... Nf6 $6 {[%eval 1.00] 1...e5 is better (+0.20).} 2. e5 $1 {[%eval 1.00]}) (1... f6 $2 {[%eval 1.50] 1...e5 is better (+0.20). Book 40%.} 2. d4 $1 {[%eval 1.50]}) 2. Nf3 {[%eval 0.20]} *", pgn)
    Assert.DoesNotContain("g5", pgn)
    agent.Close()

[<Fact>]
let ``an idea that costs more than the margin is kept when the opponent must then find only moves`` () =
    // level start: d4 is objectively best, g4 costs 0.6 - too much, unless Black must find only moves after it
    let agent =
        judge
            [ [], [ "d2d4", 0.1; "g2g4", -0.5 ]
              [ "g2g4" ], [ "d7d5", -0.5; "e7e5", 0.9 ]
              [ "g2g4"; "d7d5" ], [ "h2h3", -0.5 ]
              [ "g2g4"; "d7d5"; "h2h3" ], [ "e7e5", -0.5; "c7c6", 0.8 ]
              [ "d2d4" ], [ "d7d5", 0.1 ] ]
    let settings = { Settings.Defaults with White = true; Plies = 1; Traps = 2; Risk = 1.0 }
    let pgn = build settings agent (fun _ -> [])
    Assert.Contains("1. g4 {[%eval -0.50] Idea; objective best 1.d4 (+0.10). Black must find 1...d5! 2...e5!.} (1. d4 {[%eval 0.10] Objective best.}) *", pgn)
    // a move Black must find that only keeps Black better is a refutation, not a trap: d4 is played
    let refuted =
        judge
            [ [], [ "d2d4", 0.1; "g2g4", -0.5 ]
              [ "g2g4" ], [ "d7d5", -0.5; "e7e5", 0.3 ]
              [ "g2g4"; "d7d5" ], [ "h2h3", -0.5 ]
              [ "g2g4"; "d7d5"; "h2h3" ], [ "e7e5", -0.5; "c7c6", 0.3 ]
              [ "d2d4" ], [ "d7d5", 0.1 ] ]
    Assert.Contains("1. d4 {[%eval 0.10]", build settings refuted (fun _ -> []))
    let without = build { settings with Traps = 0 } (judge [ [], [ "d2d4", 0.1; "g2g4", -0.5 ]; [ "d2d4" ], [ "d7d5", 0.1 ] ]) (fun _ -> [])
    Assert.Contains("1. d4 {[%eval 0.10]", without)
    agent.Close()

[<Fact>]
let ``a line never ends mid-exchange: it plays on past its length, searching where the PV runs out`` () =
    // after 1.e4 the line stops at its length (1 ply) on 1...d5 - but exd5 is a capture: on through
    // exd5, then the PV runs out mid-exchange and the judge is asked how Black goes on (Qxd5 Nc3)
    let agent =
        judge
            [ [], [ "e2e4 d7d5 e4d5", 0.3 ]
              [ "e2e4"; "d7d5"; "e4d5" ], [ "d8d5 b1c3 d5a5", 0.3 ] ]
    let pgn = build { Settings.Defaults with White = true; Plies = 1; PvPlies = 2 } agent (fun _ -> [])
    // and after 3.Nc3 the queen is attacked: on to 3...Qa5
    Assert.Contains("1. e4 {[%eval 0.30]} 1... d5 2. exd5 Qxd5 3. Nc3 Qa5 *", pgn)
    agent.Close()

[<Fact>]
let ``a line never ends with a piece hanging`` () =
    // 11.h5 attacks the bishop on g6: the line goes on (the judge is asked) until it has moved
    let before = "r3kb1r/pp1n1ppp/2p1p1b1/q7/2BP2PP/2N5/PPP2P2/R1BQK2R w KQkq - 0 11"
    let afterH5 = (BoardUtils.tryMakeMove before "h4h5").Value
    let known = dict [ (key before, [ "h4h5", 3.0 ]); (key afterH5, [ "g6e4", 3.0 ]) ]
    let search (fen: string) (k: int) =
        match known.TryGetValue(key fen) with
        | true, lines -> lines |> List.truncate k |> List.map (fun (m, e) -> { Pv = [ m ]; Eval = CP e; Policy = None })
        | _ -> []
    let agent = new SearchAgent("fake", "", None, (fun f _ k -> search f k), ignore)
    let settings = { Settings.Defaults with White = true; Plies = 1; PvPlies = 1 }
    let pgn = (OpeningRepertoire.build settings agent None None (fun _ -> []) before [] "test" |> Async.RunSynchronously).Pgn
    Assert.Contains("11. h5 {[%eval 3.00]} 11... Be4 *", pgn.Replace("\r\n", " ").Replace("\n", " "))
    agent.Close()

[<Fact>]
let ``keep critical: the mainline and the branches with a mistake, the quiet ones go`` () =
    let agent =
        judge
            [ [], [ "e2e4", 0.3 ]
              [ "e2e4" ], [ "e7e5", 0.2; "c7c5", 0.25; "g8f6", 1.0 ]
              [ "e2e4"; "e7e5" ], [ "g1f3", 0.2 ]
              [ "e2e4"; "c7c5" ], [ "g1f3", 0.25 ]
              [ "e2e4"; "f7f6" ], [ "d2d4", 1.5 ]
              [ "e2e4"; "g8f6" ], [ "e4e5", 1.0 ] ]
    let settings = { Settings.Defaults with White = true; Plies = 2; Replies = 2; Traps = 0; CriticalOnly = true }
    let pgn = build settings agent (fun fen -> if key fen = key (play [ "e2e4" ]) then [ "f7f6", 40.0 ] else [])
    // c5 (quiet) and Nf6 (only ?!) go; f6? stays with our answer
    Assert.Contains("1. e4 {[%eval 0.30]} 1... e5 {[%eval 0.20]} (1... f6 $2 {[%eval 1.50] 1...e5 is better (+0.20). Book 40%.} 2. d4 $1 {[%eval 1.50]}) 2. Nf3 {[%eval 0.20]} *", pgn)
    agent.Close()

[<Fact>]
let ``a position reached again is a transposition: named, not explored twice`` () =
    // 1.d4 Nf6 2.Nf3 d5 and 1.d4 d5 2.Nf3 Nf6 meet; the second line points at the first
    let agent =
        judge
            [ [ "d2d4" ], [ "g8f6", 0.3; "d7d5", 0.3 ]
              [ "d2d4"; "g8f6" ], [ "g1f3", 0.3 ]
              [ "d2d4"; "g8f6"; "g1f3" ], [ "d7d5", 0.3 ]
              [ "d2d4"; "g8f6"; "g1f3"; "d7d5" ], [ "c2c4", 0.3 ]
              [ "d2d4"; "d7d5" ], [ "g1f3", 0.3 ]
              [ "d2d4"; "d7d5"; "g1f3" ], [ "g8f6", 0.3 ] ]
    let settings = { Settings.Defaults with White = true; Plies = 3; Traps = 0 }
    let pgn =
        (OpeningRepertoire.build settings agent None None (fun _ -> []) start [ "d2d4" ] "test" |> Async.RunSynchronously).Pgn.Replace("\r\n", " ").Replace("\n", " ")
    Assert.Contains("(1... d5 {[%eval 0.30]} 2. Nf3 {[%eval 0.30]} 2... Nf6 {[%eval 0.30] Transposes to 1...Nf6 2.Nf3 d5.})", pgn)
    Assert.Equal(1, System.Text.RegularExpressions.Regex.Matches(pgn, "c4").Count)
    agent.Close()

[<Fact>]
let ``taking back what we just took is no only move`` () =
    // after 2.exd5 only 2...Qxd5 holds, but anyone finds it: no !, no "Only move", no longer line
    let agent =
        judge
            [ [ "e2e4"; "d7d5" ], [ "e4d5", 0.3 ]
              [ "e2e4"; "d7d5"; "e4d5" ], [ "d8d5", 0.3; "c7c6", 1.2 ]
              [ "e2e4"; "d7d5"; "e4d5"; "d8d5" ], [ "b1c3", 0.3 ]
              [ "e2e4"; "d7d5"; "e4d5"; "c7c6" ], [ "d5c6", 1.2 ] ]
    let settings = { Settings.Defaults with White = true; Plies = 2; Traps = 0 }
    let pgn =
        (OpeningRepertoire.build settings agent None None (fun _ -> []) start [ "e2e4"; "d7d5" ] "test" |> Async.RunSynchronously).Pgn.Replace("\r\n", " ").Replace("\n", " ")
    Assert.Contains("2... Qxd5 {[%eval 0.30]}", pgn)
    Assert.DoesNotContain("Only move", pgn)
    agent.Close()

[<Fact>]
let ``variations carry their move numbers, a black move after a comment or variation too`` () =
    let n uci san next = { Uci = uci; San = san; Nag = ""; Comment = ""; Critical = false; Transposes = ""; Next = next }
    let tree = [ n "e2e4" "e4" [ n "e7e5" "e5" [ n "g1f3" "Nf3" [] ]; n "c7c5" "c5" [] ] ]
    Assert.Equal("1. e4 e5 (1... c5) 2. Nf3 *", (movetext start tree).Trim())

// ---------------------------------------------------------------- book bot

/// The book's moves (UCI) of the position after `moves`.
let private bookMoves (book: BotBook) (moves: string list) =
    let fen = play moves
    let key = PolyglotFormat.keyOfFen fen
    book.Entries |> Array.filter (fun e -> e.Key = key) |> Array.map (fun e -> PolyglotFormat.moveToUci fen e.Move) |> Array.sort |> List.ofArray

let private botBook settings agent played = OpeningRepertoire.botBook settings agent played |> Async.RunSynchronously

[<Fact>]
let ``a bot book holds our best move and those within reach for both sides, answering the engine's replies and the games'`` () =
    let agent =
        judge
            [ [], [ "e2e4", 0.30; "d2d4", 0.25; "c2c4", 0.10 ]
              [ "e2e4" ], [ "e7e5", 0.30; "c7c5", 0.35; "g7g5", 3.0 ]
              [ "e2e4"; "e7e5" ], [ "g1f3", 0.30 ]
              [ "e2e4"; "c7c5" ], [ "g1f3", 0.35 ]
              [ "e2e4"; "b7b6" ], [ "d2d4", 0.70 ]
              [ "d2d4" ], [ "d7d5", 0.25 ]
              [ "d2d4"; "d7d5" ], [ "c2c4", 0.25 ] ]
    // the games: 1.e4 b6 played twice against the bot
    let played (key: uint64) = if key = PolyglotFormat.keyOfFen (play [ "e2e4" ]) then [ "b7b6", 2 ] else []
    let settings = { BotBookSettings.Defaults with Plies = 3; First = [ "e2e4" ]; NarrowFrom = 8 }
    let book = botBook settings agent played
    // White: e4 and d4 (0.05 from the best), not c4; after 1.e4 e5 / c5 (within the window) and b6 (the games), not g5
    Assert.Equal<string list>([ "d2d4"; "e2e4" ], bookMoves book [])
    Assert.Equal<string list>([ "g1f3" ], bookMoves book [ "e2e4"; "e7e5" ])
    Assert.Equal<string list>([ "g1f3" ], bookMoves book [ "e2e4"; "c7c5" ])
    Assert.Equal<string list>([ "d2d4" ], bookMoves book [ "e2e4"; "b7b6" ])
    Assert.Empty(bookMoves book [ "e2e4"; "g7g5" ])
    // Black answers 1.e4 with e5 and c5 (0.05 apart for Black)
    Assert.Equal<string list>([ "c7c5"; "e7e5" ], bookMoves book [ "e2e4" ])
    let pgn = book.Pgn.Replace("\r\n", " ").Replace("\n", " ")
    Assert.Contains("[Event \"Book for White\"]", pgn)
    Assert.Contains("[Event \"Book for Black\"]", pgn)
    Assert.Contains("1... b6 {Played 2x.} 2. d4 {[%eval 0.70]}", pgn)
    agent.Close()

[<Fact>]
let ``from narrow-from on a bot book holds our best move only and follows the opponent's best reply`` () =
    let agent =
        judge
            [ [], [ "e2e4", 0.30 ]
              [ "e2e4" ], [ "e7e5", 0.30; "c7c5", 0.35 ]
              [ "e2e4"; "e7e5" ], [ "g1f3", 0.30; "b1c3", 0.28 ]
              [ "e2e4"; "c7c5" ], [ "g1f3", 0.35; "b1c3", 0.33 ] ]
    let settings = { BotBookSettings.Defaults with Plies = 3; First = [ "e2e4" ]; NarrowFrom = 1 }
    let book = botBook settings agent (fun _ -> [])
    // from ply 1 on: Black's best reply e5 only, and our best Nf3 only (Nc3 is within reach but narrow)
    Assert.Equal<string list>([ "g1f3" ], bookMoves book [ "e2e4"; "e7e5" ])
    Assert.Empty(bookMoves book [ "e2e4"; "c7c5" ])
    agent.Close()

[<Fact>]
let ``a castle in a bot book reads back as the castle`` () =
    let line = [ "e2e4"; "e7e5"; "g1f3"; "b8c6"; "f1c4"; "f8c5" ]
    let agent = judge [ for i in 0 .. line.Length do yield List.truncate i line, (if i < line.Length then [ line.[i], 0.3 ] else [ "e1g1", 0.3 ]) ]
    let book = botBook { BotBookSettings.Defaults with Plies = 7; First = [] } agent (fun _ -> [])
    Assert.Equal<string list>([ "e1g1" ], bookMoves book line)
    // stored as Polyglot writes a castle (the king takes its rook), as every Polyglot reader expects it
    let key = PolyglotFormat.keyOfFen (play line)
    let stored = book.Entries |> Array.find (fun e -> e.Key = key)
    Assert.Equal(PolyglotFormat.encodeMove "e1g1" true, stored.Move)
    Assert.NotEqual(PolyglotFormat.encodeMove "e1g1" false, stored.Move)
    // written and read back as the bot reads its book
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"eb_botbook_{System.Guid.NewGuid():N}.bin")
    try
        PolyglotFormat.write path book.Entries |> ignore
        let moves = PolyglotLines.movesFor path (play line)
        Assert.Equal<string list>([ "e1g1" ], moves |> Array.map (fun m -> m.Uci) |> List.ofArray)
    finally
        try System.IO.File.Delete path with _ -> ()
    agent.Close()

[<Fact>]
let ``the moves of a bot's opponents are read from its games, Chess960 games skipped`` () =
    let game (white: string) (black: string) (variant: string) (moves: string) =
        let tags = if variant = "" then "" else $"[Variant \"{variant}\"]\n[FEN \"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1\"]\n"
        FullPGNParser.parseFullPgnGame $"[White \"{white}\"]\n[Black \"{black}\"]\n[Result \"*\"]\n{tags}\n{moves} *\n"
    let games =
        [ game "lepbot" "Alice" "" "1. e4 c5 2. Nf3"
          game "Bob" "lepbot" "" "1. d4 Nf6 2. c4"
          game "Carol" "lepbot" "" "1. d4 d5"
          game "Dave" "lepbot" "chess960" "1. b3 e5" ]
    let played = PolyglotLines.playedMoves games 16 "lepbot"
    let at moves = match played.TryGetValue(PolyglotFormat.keyOfFen (play moves)) with | true, m -> m | _ -> []
    // the opponents' moves only: 1...c5, 1.d4 twice, 2.c4; the bot's own and the Chess960 game's not
    Assert.Equal<(string * int) list>([ "d2d4", 2 ], at [])
    Assert.Equal<(string * int) list>([ "c7c5", 1 ], at [ "e2e4" ])
    Assert.Equal<(string * int) list>([ "c2c4", 1 ], at [ "d2d4"; "g8f6" ])
    Assert.Empty(at [ "d2d4" ])
    Assert.Empty(at [ "e2e4"; "c7c5" ])

[<Fact>]
let ``with a policy search a bot book takes our moves near the top policy and the opponent's replies above the minimum`` () =
    // one node: every move carries the policy and the position's eval
    let table =
        [ [], [ "e2e4", 0.40; "d2d4", 0.25; "c2c4", 0.15 ]
          [ "e2e4" ], [ "e7e5", 0.30; "c7c5", 0.25; "e7e6", 0.08 ]
          [ "d2d4" ], [ "g8f6", 0.50 ] ]
    let known = table |> List.map (fun (moves, lines) -> key (play moves), lines) |> dict
    let search (fen: string) (_: int) =
        match known.TryGetValue(key fen) with
        | true, lines -> lines |> List.map (fun (m, p) -> { Pv = [ m ]; Eval = CP 0.2; Policy = Some p })
        | _ -> []
    let agent = new SearchAgent("fake", "", None, (fun f _ k -> search f k), ignore)
    let settings = { BotBookSettings.Defaults with Plies = 2; First = []; PolicyWithin = 0.5; MinPolicy = 0.10 }
    let book = botBook settings agent (fun _ -> [])
    // ours: e4 (40%) and d4 (25%, at least half of it), not c4 (15%)
    Assert.Equal<string list>([ "d2d4"; "e2e4" ], bookMoves book [])
    // Black's book: White's replies above 10% are answered (e4, d4, c4 - c4 has no position, so no move)
    Assert.Equal<string list>([ "e7e5"; "c7c5" ] |> List.sort, bookMoves book [ "e2e4" ])
    Assert.Equal<string list>([ "g8f6" ], bookMoves book [ "d2d4" ])
    let pgn = book.Pgn.Replace("\r\n", " ").Replace("\n", " ")
    Assert.Contains("1. e4 {P 40%}", pgn)
    Assert.DoesNotContain("%eval", pgn)
    agent.Close()

[<Fact>]
let ``with a human net the most played replies are explored, a popular blunder is answered however bad`` () =
    let lines =
        [ [], [ "e2e4", 0.3 ]
          [ "e2e4" ], [ "e7e5", 0.2; "c7c5", 0.25; "d7d6", 0.3; "g7g5", 4.0 ]
          [ "e2e4"; "e7e5" ], [ "g1f3", 0.2 ]
          [ "e2e4"; "e7e5"; "g1f3" ], [ "b8c6", 0.2 ]
          [ "e2e4"; "c7c5" ], [ "g1f3", 0.25 ]
          [ "e2e4"; "c7c5"; "g1f3" ], [ "d7d6", 0.25 ]
          [ "e2e4"; "d7d6" ], [ "d2d4", 0.3 ]
          [ "e2e4"; "d7d6"; "d2d4" ], [ "g8f6", 0.3 ]
          [ "e2e4"; "g7g5" ], [ "d2d4", 4.0 ] ]
    let settings = { Settings.Defaults with White = true; Plies = 4; Replies = 2; Traps = 0 }
    let run human =
        let agent = judge lines
        let pgn = (OpeningRepertoire.build settings agent None human (fun _ -> []) start [] "test" |> Async.RunSynchronously).Pgn.Replace("\r\n", " ").Replace("\n", " ")
        agent.Close()
        pgn
    // without it: c5, the nearer to the best, is explored and g5 (3.8 worse) left out
    let engineOnly = run None
    Assert.Contains("2... d6", engineOnly)
    Assert.DoesNotContain("Nf6", engineOnly)
    Assert.DoesNotContain("g5", engineOnly)
    // people play d6 and g5 most: d6 is explored instead of c5, g5 answered and marked
    let shares = [ "d7d6", 0.5; "g7g5", 0.2; "e7e5", 0.15; "c7c5", 0.1; "a7a6", 0.05 ]
    // the net gets the game from its start, not the position alone: Maia reads the history
    let heard = System.Collections.Generic.List<string>()
    let maia =
        new SearchAgent("Maia-1500", "", None, (fun fen moves _ -> heard.Add(String.concat " " moves); if key fen = key start && moves = [ "e2e4" ] then shares |> List.map (fun (m, p) -> { Pv = [ m ]; Eval = CP 0.0; Policy = Some p }) else []), ignore)
    let human = run (Some maia)
    maia.Close()
    Assert.Contains("e2e4", heard)
    Assert.Contains("1... d6 {[%eval 0.30] Maia-1500 50%.} 2. d4 {[%eval 0.30]} 2... Nf6", human)
    Assert.DoesNotContain("2... d6", human)
    Assert.Contains("1... g5 $4 {[%eval 4.00] 1...e5 is better (+0.20). Maia-1500 20%.} 2. d4 $1", human)
    Assert.Contains("[Annotator \"fake, people Maia-1500\"]", (OpeningRepertoire.build settings (judge lines) None (Some(new SearchAgent("Maia-1500", "", None, (fun _ _ _ -> []), ignore))) (fun _ -> []) start [] "test" |> Async.RunSynchronously).Pgn)

[<Fact>]
let ``an eval that rounds to zero from below is written 0.00, not -0.00`` () =
    // the best reply leaves White -0.001: the punished g5 names it as +0.00
    let agent =
        judge
            [ [], [ "e2e4", 0.3 ]
              [ "e2e4" ], [ "e7e5", -0.001; "g7g5", 1.0 ]
              [ "e2e4"; "e7e5" ], [ "g1f3", -0.001 ]
              [ "e2e4"; "g7g5" ], [ "d2d4", 1.0 ] ]
    let pgn = build { Settings.Defaults with White = true; Plies = 2; Traps = 0 } agent (fun _ -> [])
    Assert.Contains("1... e5 $1 {[%eval 0.00] Only move; 1...g5 +1.00.}", pgn)
    Assert.Contains("1...e5 is better (+0.00).", pgn)
    Assert.DoesNotContain("-0.00", pgn)
    agent.Close()

[<Fact>]
let ``a search from moves that do not play fails that search, and the agent goes on answering`` () =
    let agent = judge [ [], [ "e2e4", 0.3 ] ]
    // e2e5 is no move: an error for this search (it must not stop the agent, which would never answer
    // again). Waited for with a limit, so a dead agent fails the test instead of hanging it.
    let bad = Async.StartAsTask(agent.SearchFrom(start, [ "e2e5" ], 1))
    let failed = try (bad.Wait 5000 |> ignore; false) with _ -> true
    Assert.True(failed, "the search from a move that does not play did not fail")
    let good = Async.StartAsTask(agent.Search(start, 1))
    Assert.True(good.Wait 5000, "the agent stopped answering")
    Assert.Equal<string list>([ "e2e4" ], good.Result.Head.Pv)
    agent.Close()

[<Fact>]
let ``the trainer reads what the writer writes: the side from the Event, our alternatives from their label`` () =
    // Black's level answer to 1.e4: c5 is played, e5 within the margin stays beside it as an alternative
    let agent =
        judge
            [ [ "e2e4" ], [ "c7c5", -0.1; "e7e5", -0.05 ]
              [ "e2e4"; "c7c5" ], [ "g1f3", -0.1 ]
              [ "e2e4"; "e7e5" ], [ "g1f3", -0.05 ] ]
    let settings = { Settings.Defaults with White = false; Plies = 2; Traps = 0 }
    let pgn = (OpeningRepertoire.build settings agent None None (fun _ -> []) start [ "e2e4" ] (title false) |> Async.RunSynchronously).Pgn
    agent.Close()
    Assert.Contains(alsoGoodLabel, pgn)
    match RepertoireTrainer.load pgn true with
    | Error e -> failwith e
    | Ok tree ->
        Assert.False tree.White
        let black = tree.Root.Head.Next
        Assert.Equal<string list>([ "c5"; "e5" ], black |> List.map (fun n -> n.San))
        Assert.Equal<bool list>([ false; true ], black |> List.map (fun n -> n.AlsoGood))
